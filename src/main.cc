#include "anchor.h"
#include "terminal.h"
#include "events.h"
#include "js/js.h"

#include <iostream>
#include <sstream>
#include <cstdlib>
#include <cassert>

#include <signal.h>
#include <event2/event.h>
#include <ncurses.h>

using namespace std;
using namespace olec;
using namespace olec::js;

struct EventDispatcherWrap: EventDispatcher {
	static
	void cb_reload(int, short, EventDispatcherWrap* self) {
		self->reload();
	}

	static
	void cb_mouse(int, short, EventDispatcherWrap* self) {
		getch();

		// int ch = getch();

		// logdebug("getch: %i (expect %i)", ch, KEY_MOUSE);

		// if (ch == KEY_MOUSE) {
		// 	MEVENT mouse_event;
		// 	getmouse(&mouse_event);

		// 	logdebug("Mouse: coords = %ix%i, state = %i",
		// 	         mouse_event.x, mouse_event.y, mouse_event.bstate);
		// }
	}

	v8::Isolate* isolate;
	v8::UniquePersistent<v8::Object> key_handler;

	struct event* ev_reload;
	struct event* ev_mouse;

	bool loop = false;

	EventDispatcherWrap(v8::Isolate* isolate, int fd):
		EventDispatcher(fd),
		isolate(isolate)
	{
		key_handler.Reset(isolate, FunctionTemplate<void>(isolate, [](){})->GetFunction());

		ev_reload = event_new(ev_base, SIGUSR1, EV_PERSIST | EV_SIGNAL,
	                          (event_callback_fn) cb_reload, this);
		assert(ev_reload != nullptr);

		ev_mouse = event_new(ev_base, STDIN_FILENO, EV_PERSIST | EV_READ,
	                         (event_callback_fn) cb_mouse, this);
		assert(ev_mouse != nullptr);
	}

	~EventDispatcherWrap() {
		event_free(ev_reload);
		event_free(ev_mouse);
	}

	void dispatch() {
		event_add(ev_reload, nullptr);
		event_add(ev_mouse, nullptr);

		EventDispatcher::dispatch();
	}

	void quit() {
		logdebug("Quit event dispatcher");

		event_del(ev_reload);
		event_del(ev_mouse);

		EventDispatcher::quit();
		loop = false;
	}

	void reload() {
		logdebug("Reload application");

		quit();
		loop = true;
	}

	void event(const Event& ev) {
		if (ev.type == Event::KeyPress) {
			v8::Local<v8::Value> params[2] = {
				v8::Uint32::NewFromUnsigned(isolate, ev.info.key_press.mod),
				v8::Uint32::NewFromUnsigned(isolate, ev.info.key_press.key)
			};

			auto eh = v8::Local<v8::Object>::New(isolate, key_handler);
			eh->CallAsFunction(v8::Null(isolate), 2, params);
		}
	}

	void resize(const winsize& ws) {
		if (stdscr != nullptr && !isendwin()) {
			logdebug("Resize terminal %ix%i", ws.ws_col, ws.ws_row);
			resizeterm(ws.ws_row, ws.ws_col);
		}
	}

	v8::Local<v8::Object> getKeyHandler() {
		return v8::Local<v8::Object>::New(isolate, key_handler);
	}

	void setKeyHandler(v8::Local<v8::Object> eh) {
		if (!eh.IsEmpty() && eh->IsCallable()) {
			key_handler.Reset(isolate, eh);
		} else {
			v8::String::Utf8Value strval(eh);
			logwarn("Provided key handler '%s' is not callable", *strval);
		}
	}
};

using NCursesWindow = WINDOW;

struct Frame {
	ClassBuilder<Frame>& type_template;
	NCursesWindow* screen;

	Frame(ClassBuilder<Frame>& tt):
		type_template(tt)
	{
		initscr();
		start_color();
		noecho();
		raw();

		keypad(stdscr, true);
		mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, nullptr);

		screen = stdscr;

		if (!can_change_color())
			logwarn("Cannot change colors");
	}

	Frame(ClassBuilder<Frame>& tt,
	      NCursesWindow* scr):
		type_template(tt),
		screen(scr)
	{}

	~Frame() {
		if (screen == stdscr) {
			endwin();
		} else {
			delwin(screen);
		}
	}

	void moveCursor(UnsignedInteger x, UnsignedInteger y) {
		wmove(screen, y, x);
	}

	void setStyle(UnsignedInteger attrs, UnsignedInteger pair) {
		wattrset(screen, attrs | COLOR_PAIR(pair));
	}

	void drawString(String str) {
		waddstr(screen, str.c_str());
	}

	void clear() {
		wclear(screen);
	}

	void render() {
		wrefresh(screen);
	}

	UnsignedInteger getWidth() {
		return getmaxx(screen);
	}

	UnsignedInteger getHeight() {
		return getmaxy(screen);
	}

	UnsignedInteger getX() {
		return getparx(screen);
	}

	UnsignedInteger getY() {
		return getpary(screen);
	}

	v8::Local<v8::Value> createSubFrame(UnsignedInteger x, UnsignedInteger y,
	                                    UnsignedInteger w, UnsignedInteger h) {
		NCursesWindow* sub_screen = derwin(screen, h, w, y, x);

		if (sub_screen) {
			return type_template.instantiate(type_template, sub_screen);
		} else {
			logerror("Failed to create sub window; parent = %p, rect = (%i, %i, %i, %i)",
			         screen, x, y, w, h);
			return v8::Null(v8::Isolate::GetCurrent());
		}
	}

	v8::Local<v8::Value> getRoot() {
		return type_template.instantiate(type_template, wgetparent(screen));
	}

	bool setBounds(UnsignedInteger x, UnsignedInteger y, UnsignedInteger w, UnsignedInteger h) {
		if (screen == stdscr) {
			logwarn("Cannot set bounds of standard screen");
			return false;
		}

		NCursesWindow* new_window = derwin(wgetparent(screen), h, w, y, x);

		if (!new_window) {
			logerror("Failed to set bounds");
			return false;
		}

		wclear(screen);
		wrefresh(screen);

		delwin(screen);
		screen = new_window;

		return true;
	}
};

int main(int argc, char** argv) {
	// Enable 256 colors
	putenv((char*) "TERM=xterm-256color");

	// Fork process
	assert(argc > 0);
	Anchor a(argv[0]);

	if (a) {
		gtk_init(&argc, &argv);

		Terminal term(a);
		term.show();

		gtk_main();
	} else {
		// Initialize V8
		EngineInstance vm;
		vm.isolate->SetFatalErrorHandler([](const char* location, const char* message) {
			logerror("[%s] %s", location, message);
		});

		// Event dispatcher wrapper
		ClassBuilder<EventDispatcherWrap> events_tpls(vm);

		events_tpls.method("dispatch", &EventDispatcherWrap::dispatch);
		events_tpls.method("quit", &EventDispatcherWrap::quit);
		events_tpls.method("reload", &EventDispatcherWrap::reload);
		events_tpls.property("keyHandler",
		                    &EventDispatcherWrap::getKeyHandler,
		                    &EventDispatcherWrap::setKeyHandler);

		// Key codes
		events_tpls.instance.setForeign("BackSpace", UnsignedInteger(GDK_KEY_BackSpace));
		events_tpls.instance.setForeign("Tab", UnsignedInteger(GDK_KEY_Tab));
		events_tpls.instance.setForeign("Linefeed", UnsignedInteger(GDK_KEY_Linefeed));
		events_tpls.instance.setForeign("Clear", UnsignedInteger(GDK_KEY_Clear));
		events_tpls.instance.setForeign("Return", UnsignedInteger(GDK_KEY_Return));
		events_tpls.instance.setForeign("Pause", UnsignedInteger(GDK_KEY_Pause));
		events_tpls.instance.setForeign("ScrollLock", UnsignedInteger(GDK_KEY_Scroll_Lock));
		events_tpls.instance.setForeign("SysReq", UnsignedInteger(GDK_KEY_Sys_Req));
		events_tpls.instance.setForeign("Escape", UnsignedInteger(GDK_KEY_Escape));
		events_tpls.instance.setForeign("Delete", UnsignedInteger(GDK_KEY_Delete));
		events_tpls.instance.setForeign("Home", UnsignedInteger(GDK_KEY_Home));
		events_tpls.instance.setForeign("Left", UnsignedInteger(GDK_KEY_Left));
		events_tpls.instance.setForeign("Up", UnsignedInteger(GDK_KEY_Up));
		events_tpls.instance.setForeign("Right", UnsignedInteger(GDK_KEY_Right));
		events_tpls.instance.setForeign("Down", UnsignedInteger(GDK_KEY_Down));
		events_tpls.instance.setForeign("Prior", UnsignedInteger(GDK_KEY_Prior));
		events_tpls.instance.setForeign("PageUp", UnsignedInteger(GDK_KEY_Page_Up));
		events_tpls.instance.setForeign("Next", UnsignedInteger(GDK_KEY_Next));
		events_tpls.instance.setForeign("PageDown", UnsignedInteger(GDK_KEY_Page_Down));
		events_tpls.instance.setForeign("End", UnsignedInteger(GDK_KEY_End));
		events_tpls.instance.setForeign("Begin", UnsignedInteger(GDK_KEY_Begin));
		events_tpls.instance.setForeign("Select", UnsignedInteger(GDK_KEY_Select));
		events_tpls.instance.setForeign("Print", UnsignedInteger(GDK_KEY_Print));
		events_tpls.instance.setForeign("Execute", UnsignedInteger(GDK_KEY_Execute));
		events_tpls.instance.setForeign("Insert", UnsignedInteger(GDK_KEY_Insert));
		events_tpls.instance.setForeign("Undo", UnsignedInteger(GDK_KEY_Undo));
		events_tpls.instance.setForeign("Redo", UnsignedInteger(GDK_KEY_Redo));
		events_tpls.instance.setForeign("Menu", UnsignedInteger(GDK_KEY_Menu));
		events_tpls.instance.setForeign("Find", UnsignedInteger(GDK_KEY_Find));
		events_tpls.instance.setForeign("Cancel", UnsignedInteger(GDK_KEY_Cancel));
		events_tpls.instance.setForeign("Help", UnsignedInteger(GDK_KEY_Help));
		events_tpls.instance.setForeign("Break", UnsignedInteger(GDK_KEY_Break));
		events_tpls.instance.setForeign("Num_Lock", UnsignedInteger(GDK_KEY_Num_Lock));
		events_tpls.instance.setForeign("KPSpace", UnsignedInteger(GDK_KEY_KP_Space));
		events_tpls.instance.setForeign("KPTab", UnsignedInteger(GDK_KEY_KP_Tab));
		events_tpls.instance.setForeign("KPEnter", UnsignedInteger(GDK_KEY_KP_Enter));
		events_tpls.instance.setForeign("KPF1", UnsignedInteger(GDK_KEY_KP_F1));
		events_tpls.instance.setForeign("KPF2", UnsignedInteger(GDK_KEY_KP_F2));
		events_tpls.instance.setForeign("KPF3", UnsignedInteger(GDK_KEY_KP_F3));
		events_tpls.instance.setForeign("KPF4", UnsignedInteger(GDK_KEY_KP_F4));
		events_tpls.instance.setForeign("KPHome", UnsignedInteger(GDK_KEY_KP_Home));
		events_tpls.instance.setForeign("KPLeft", UnsignedInteger(GDK_KEY_KP_Left));
		events_tpls.instance.setForeign("KPUp", UnsignedInteger(GDK_KEY_KP_Up));
		events_tpls.instance.setForeign("KPRight", UnsignedInteger(GDK_KEY_KP_Right));
		events_tpls.instance.setForeign("KPDown", UnsignedInteger(GDK_KEY_KP_Down));
		events_tpls.instance.setForeign("KPPrior", UnsignedInteger(GDK_KEY_KP_Prior));
		events_tpls.instance.setForeign("KPPage_Up", UnsignedInteger(GDK_KEY_KP_Page_Up));
		events_tpls.instance.setForeign("KPNext", UnsignedInteger(GDK_KEY_KP_Next));
		events_tpls.instance.setForeign("KPPage_Down", UnsignedInteger(GDK_KEY_KP_Page_Down));
		events_tpls.instance.setForeign("KPEnd", UnsignedInteger(GDK_KEY_KP_End));
		events_tpls.instance.setForeign("KPBegin", UnsignedInteger(GDK_KEY_KP_Begin));
		events_tpls.instance.setForeign("KPInsert", UnsignedInteger(GDK_KEY_KP_Insert));
		events_tpls.instance.setForeign("KPDelete", UnsignedInteger(GDK_KEY_KP_Delete));
		events_tpls.instance.setForeign("KPEqual", UnsignedInteger(GDK_KEY_KP_Equal));
		events_tpls.instance.setForeign("KPMultiply", UnsignedInteger(GDK_KEY_KP_Multiply));
		events_tpls.instance.setForeign("KPAdd", UnsignedInteger(GDK_KEY_KP_Add));
		events_tpls.instance.setForeign("KPSeparator", UnsignedInteger(GDK_KEY_KP_Separator));
		events_tpls.instance.setForeign("KPSubtract", UnsignedInteger(GDK_KEY_KP_Subtract));
		events_tpls.instance.setForeign("KPDecimal", UnsignedInteger(GDK_KEY_KP_Decimal));
		events_tpls.instance.setForeign("KPDivide", UnsignedInteger(GDK_KEY_KP_Divide));
		events_tpls.instance.setForeign("KP0", UnsignedInteger(GDK_KEY_KP_0));
		events_tpls.instance.setForeign("KP1", UnsignedInteger(GDK_KEY_KP_1));
		events_tpls.instance.setForeign("KP2", UnsignedInteger(GDK_KEY_KP_2));
		events_tpls.instance.setForeign("KP3", UnsignedInteger(GDK_KEY_KP_3));
		events_tpls.instance.setForeign("KP4", UnsignedInteger(GDK_KEY_KP_4));
		events_tpls.instance.setForeign("KP5", UnsignedInteger(GDK_KEY_KP_5));
		events_tpls.instance.setForeign("KP6", UnsignedInteger(GDK_KEY_KP_6));
		events_tpls.instance.setForeign("KP7", UnsignedInteger(GDK_KEY_KP_7));
		events_tpls.instance.setForeign("KP8", UnsignedInteger(GDK_KEY_KP_8));
		events_tpls.instance.setForeign("KP9", UnsignedInteger(GDK_KEY_KP_9));
		events_tpls.instance.setForeign("F1", UnsignedInteger(GDK_KEY_F1));
		events_tpls.instance.setForeign("F2", UnsignedInteger(GDK_KEY_F2));
		events_tpls.instance.setForeign("F3", UnsignedInteger(GDK_KEY_F3));
		events_tpls.instance.setForeign("F4", UnsignedInteger(GDK_KEY_F4));
		events_tpls.instance.setForeign("F5", UnsignedInteger(GDK_KEY_F5));
		events_tpls.instance.setForeign("F6", UnsignedInteger(GDK_KEY_F6));
		events_tpls.instance.setForeign("F7", UnsignedInteger(GDK_KEY_F7));
		events_tpls.instance.setForeign("F8", UnsignedInteger(GDK_KEY_F8));
		events_tpls.instance.setForeign("F9", UnsignedInteger(GDK_KEY_F9));
		events_tpls.instance.setForeign("F10", UnsignedInteger(GDK_KEY_F10));
		events_tpls.instance.setForeign("F11", UnsignedInteger(GDK_KEY_F11));
		events_tpls.instance.setForeign("F12", UnsignedInteger(GDK_KEY_F12));
		events_tpls.instance.setForeign("F13", UnsignedInteger(GDK_KEY_F13));
		events_tpls.instance.setForeign("F14", UnsignedInteger(GDK_KEY_F14));
		events_tpls.instance.setForeign("F15", UnsignedInteger(GDK_KEY_F15));
		events_tpls.instance.setForeign("F16", UnsignedInteger(GDK_KEY_F16));
		events_tpls.instance.setForeign("F17", UnsignedInteger(GDK_KEY_F17));
		events_tpls.instance.setForeign("F18", UnsignedInteger(GDK_KEY_F18));
		events_tpls.instance.setForeign("F19", UnsignedInteger(GDK_KEY_F19));
		events_tpls.instance.setForeign("F20", UnsignedInteger(GDK_KEY_F20));
		events_tpls.instance.setForeign("F21", UnsignedInteger(GDK_KEY_F21));
		events_tpls.instance.setForeign("F22", UnsignedInteger(GDK_KEY_F22));
		events_tpls.instance.setForeign("F23", UnsignedInteger(GDK_KEY_F23));
		events_tpls.instance.setForeign("F24", UnsignedInteger(GDK_KEY_F24));
		events_tpls.instance.setForeign("F25", UnsignedInteger(GDK_KEY_F25));
		events_tpls.instance.setForeign("F26", UnsignedInteger(GDK_KEY_F26));
		events_tpls.instance.setForeign("F27", UnsignedInteger(GDK_KEY_F27));
		events_tpls.instance.setForeign("F28", UnsignedInteger(GDK_KEY_F28));
		events_tpls.instance.setForeign("F29", UnsignedInteger(GDK_KEY_F29));
		events_tpls.instance.setForeign("F30", UnsignedInteger(GDK_KEY_F30));
		events_tpls.instance.setForeign("F31", UnsignedInteger(GDK_KEY_F31));
		events_tpls.instance.setForeign("F32", UnsignedInteger(GDK_KEY_F32));
		events_tpls.instance.setForeign("F33", UnsignedInteger(GDK_KEY_F33));
		events_tpls.instance.setForeign("F34", UnsignedInteger(GDK_KEY_F34));
		events_tpls.instance.setForeign("F35", UnsignedInteger(GDK_KEY_F35));
		events_tpls.instance.setForeign("ModeLock", UnsignedInteger(GDK_KEY_ModeLock));
		events_tpls.instance.setForeign("MonBrightnessUp", UnsignedInteger(GDK_KEY_MonBrightnessUp));
		events_tpls.instance.setForeign("MonBrightnessDown", UnsignedInteger(GDK_KEY_MonBrightnessDown));
		events_tpls.instance.setForeign("KbdLightOnOff", UnsignedInteger(GDK_KEY_KbdLightOnOff));
		events_tpls.instance.setForeign("KbdBrightnessUp", UnsignedInteger(GDK_KEY_KbdBrightnessUp));
		events_tpls.instance.setForeign("KbdBrightnessDown", UnsignedInteger(GDK_KEY_KbdBrightnessDown));
		events_tpls.instance.setForeign("Standby", UnsignedInteger(GDK_KEY_Standby));
		events_tpls.instance.setForeign("AudioLowerVolume", UnsignedInteger(GDK_KEY_AudioLowerVolume));
		events_tpls.instance.setForeign("AudioMute", UnsignedInteger(GDK_KEY_AudioMute));
		events_tpls.instance.setForeign("AudioRaiseVolume", UnsignedInteger(GDK_KEY_AudioRaiseVolume));
		events_tpls.instance.setForeign("AudioPlay", UnsignedInteger(GDK_KEY_AudioPlay));
		events_tpls.instance.setForeign("AudioStop", UnsignedInteger(GDK_KEY_AudioStop));
		events_tpls.instance.setForeign("AudioPrev", UnsignedInteger(GDK_KEY_AudioPrev));
		events_tpls.instance.setForeign("AudioNext", UnsignedInteger(GDK_KEY_AudioNext));
		events_tpls.instance.setForeign("HomePage", UnsignedInteger(GDK_KEY_HomePage));
		events_tpls.instance.setForeign("Mail", UnsignedInteger(GDK_KEY_Mail));
		events_tpls.instance.setForeign("Start", UnsignedInteger(GDK_KEY_Start));
		events_tpls.instance.setForeign("Search", UnsignedInteger(GDK_KEY_Search));
		events_tpls.instance.setForeign("AudioRecord", UnsignedInteger(GDK_KEY_AudioRecord));
		events_tpls.instance.setForeign("Calculator", UnsignedInteger(GDK_KEY_Calculator));
		events_tpls.instance.setForeign("Memo", UnsignedInteger(GDK_KEY_Memo));
		events_tpls.instance.setForeign("ToDoList", UnsignedInteger(GDK_KEY_ToDoList));
		events_tpls.instance.setForeign("Calendar", UnsignedInteger(GDK_KEY_Calendar));
		events_tpls.instance.setForeign("PowerDown", UnsignedInteger(GDK_KEY_PowerDown));
		events_tpls.instance.setForeign("ContrastAdjust", UnsignedInteger(GDK_KEY_ContrastAdjust));
		events_tpls.instance.setForeign("RockerUp", UnsignedInteger(GDK_KEY_RockerUp));
		events_tpls.instance.setForeign("RockerDown", UnsignedInteger(GDK_KEY_RockerDown));
		events_tpls.instance.setForeign("RockerEnter", UnsignedInteger(GDK_KEY_RockerEnter));
		events_tpls.instance.setForeign("Back", UnsignedInteger(GDK_KEY_Back));
		events_tpls.instance.setForeign("Forward", UnsignedInteger(GDK_KEY_Forward));
		events_tpls.instance.setForeign("Stop", UnsignedInteger(GDK_KEY_Stop));
		events_tpls.instance.setForeign("Refresh", UnsignedInteger(GDK_KEY_Refresh));
		events_tpls.instance.setForeign("PowerOff", UnsignedInteger(GDK_KEY_PowerOff));
		events_tpls.instance.setForeign("WakeUp", UnsignedInteger(GDK_KEY_WakeUp));
		events_tpls.instance.setForeign("Eject", UnsignedInteger(GDK_KEY_Eject));
		events_tpls.instance.setForeign("ScreenSaver", UnsignedInteger(GDK_KEY_ScreenSaver));
		events_tpls.instance.setForeign("WWW", UnsignedInteger(GDK_KEY_WWW));
		events_tpls.instance.setForeign("Sleep", UnsignedInteger(GDK_KEY_Sleep));
		events_tpls.instance.setForeign("Favorites", UnsignedInteger(GDK_KEY_Favorites));
		events_tpls.instance.setForeign("AudioPause", UnsignedInteger(GDK_KEY_AudioPause));
		events_tpls.instance.setForeign("AudioMedia", UnsignedInteger(GDK_KEY_AudioMedia));
		events_tpls.instance.setForeign("MyComputer", UnsignedInteger(GDK_KEY_MyComputer));
		events_tpls.instance.setForeign("VendorHome", UnsignedInteger(GDK_KEY_VendorHome));
		events_tpls.instance.setForeign("LightBulb", UnsignedInteger(GDK_KEY_LightBulb));
		events_tpls.instance.setForeign("Shop", UnsignedInteger(GDK_KEY_Shop));
		events_tpls.instance.setForeign("History", UnsignedInteger(GDK_KEY_History));
		events_tpls.instance.setForeign("OpenURL", UnsignedInteger(GDK_KEY_OpenURL));
		events_tpls.instance.setForeign("AddFavorite", UnsignedInteger(GDK_KEY_AddFavorite));
		events_tpls.instance.setForeign("HotLinks", UnsignedInteger(GDK_KEY_HotLinks));
		events_tpls.instance.setForeign("BrightnessAdjust", UnsignedInteger(GDK_KEY_BrightnessAdjust));
		events_tpls.instance.setForeign("Finance", UnsignedInteger(GDK_KEY_Finance));
		events_tpls.instance.setForeign("Community", UnsignedInteger(GDK_KEY_Community));
		events_tpls.instance.setForeign("AudioRewind", UnsignedInteger(GDK_KEY_AudioRewind));
		events_tpls.instance.setForeign("BackForward", UnsignedInteger(GDK_KEY_BackForward));
		events_tpls.instance.setForeign("Launch0", UnsignedInteger(GDK_KEY_Launch0));
		events_tpls.instance.setForeign("Launch1", UnsignedInteger(GDK_KEY_Launch1));
		events_tpls.instance.setForeign("Launch2", UnsignedInteger(GDK_KEY_Launch2));
		events_tpls.instance.setForeign("Launch3", UnsignedInteger(GDK_KEY_Launch3));
		events_tpls.instance.setForeign("Launch4", UnsignedInteger(GDK_KEY_Launch4));
		events_tpls.instance.setForeign("Launch5", UnsignedInteger(GDK_KEY_Launch5));
		events_tpls.instance.setForeign("Launch6", UnsignedInteger(GDK_KEY_Launch6));
		events_tpls.instance.setForeign("Launch7", UnsignedInteger(GDK_KEY_Launch7));
		events_tpls.instance.setForeign("Launch8", UnsignedInteger(GDK_KEY_Launch8));
		events_tpls.instance.setForeign("Launch9", UnsignedInteger(GDK_KEY_Launch9));
		events_tpls.instance.setForeign("LaunchA", UnsignedInteger(GDK_KEY_LaunchA));
		events_tpls.instance.setForeign("LaunchB", UnsignedInteger(GDK_KEY_LaunchB));
		events_tpls.instance.setForeign("LaunchC", UnsignedInteger(GDK_KEY_LaunchC));
		events_tpls.instance.setForeign("LaunchD", UnsignedInteger(GDK_KEY_LaunchD));
		events_tpls.instance.setForeign("LaunchE", UnsignedInteger(GDK_KEY_LaunchE));
		events_tpls.instance.setForeign("LaunchF", UnsignedInteger(GDK_KEY_LaunchF));
		events_tpls.instance.setForeign("ApplicationLeft", UnsignedInteger(GDK_KEY_ApplicationLeft));
		events_tpls.instance.setForeign("ApplicationRight", UnsignedInteger(GDK_KEY_ApplicationRight));
		events_tpls.instance.setForeign("Book", UnsignedInteger(GDK_KEY_Book));
		events_tpls.instance.setForeign("CD", UnsignedInteger(GDK_KEY_CD));
		events_tpls.instance.setForeign("WindowClear", UnsignedInteger(GDK_KEY_WindowClear));
		events_tpls.instance.setForeign("Close", UnsignedInteger(GDK_KEY_Close));
		events_tpls.instance.setForeign("Copy", UnsignedInteger(GDK_KEY_Copy));
		events_tpls.instance.setForeign("Cut", UnsignedInteger(GDK_KEY_Cut));
		events_tpls.instance.setForeign("Display", UnsignedInteger(GDK_KEY_Display));
		events_tpls.instance.setForeign("DOS", UnsignedInteger(GDK_KEY_DOS));
		events_tpls.instance.setForeign("Documents", UnsignedInteger(GDK_KEY_Documents));
		events_tpls.instance.setForeign("Excel", UnsignedInteger(GDK_KEY_Excel));
		events_tpls.instance.setForeign("Explorer", UnsignedInteger(GDK_KEY_Explorer));
		events_tpls.instance.setForeign("Game", UnsignedInteger(GDK_KEY_Game));
		events_tpls.instance.setForeign("Go", UnsignedInteger(GDK_KEY_Go));
		events_tpls.instance.setForeign("iTouch", UnsignedInteger(GDK_KEY_iTouch));
		events_tpls.instance.setForeign("LogOff", UnsignedInteger(GDK_KEY_LogOff));
		events_tpls.instance.setForeign("Market", UnsignedInteger(GDK_KEY_Market));
		events_tpls.instance.setForeign("Meeting", UnsignedInteger(GDK_KEY_Meeting));
		events_tpls.instance.setForeign("MenuKB", UnsignedInteger(GDK_KEY_MenuKB));
		events_tpls.instance.setForeign("MenuPB", UnsignedInteger(GDK_KEY_MenuPB));
		events_tpls.instance.setForeign("MySites", UnsignedInteger(GDK_KEY_MySites));
		events_tpls.instance.setForeign("New", UnsignedInteger(GDK_KEY_New));
		events_tpls.instance.setForeign("News", UnsignedInteger(GDK_KEY_News));
		events_tpls.instance.setForeign("OfficeHome", UnsignedInteger(GDK_KEY_OfficeHome));
		events_tpls.instance.setForeign("Open", UnsignedInteger(GDK_KEY_Open));
		events_tpls.instance.setForeign("Option", UnsignedInteger(GDK_KEY_Option));
		events_tpls.instance.setForeign("Paste", UnsignedInteger(GDK_KEY_Paste));
		events_tpls.instance.setForeign("Phone", UnsignedInteger(GDK_KEY_Phone));
		events_tpls.instance.setForeign("Reply", UnsignedInteger(GDK_KEY_Reply));
		events_tpls.instance.setForeign("Reload", UnsignedInteger(GDK_KEY_Reload));
		events_tpls.instance.setForeign("RotateWindows", UnsignedInteger(GDK_KEY_RotateWindows));
		events_tpls.instance.setForeign("RotationPB", UnsignedInteger(GDK_KEY_RotationPB));
		events_tpls.instance.setForeign("RotationKB", UnsignedInteger(GDK_KEY_RotationKB));
		events_tpls.instance.setForeign("Save", UnsignedInteger(GDK_KEY_Save));
		events_tpls.instance.setForeign("ScrollUp", UnsignedInteger(GDK_KEY_ScrollUp));
		events_tpls.instance.setForeign("ScrollDown", UnsignedInteger(GDK_KEY_ScrollDown));
		events_tpls.instance.setForeign("ScrollClick", UnsignedInteger(GDK_KEY_ScrollClick));
		events_tpls.instance.setForeign("Send", UnsignedInteger(GDK_KEY_Send));
		events_tpls.instance.setForeign("Spell", UnsignedInteger(GDK_KEY_Spell));
		events_tpls.instance.setForeign("SplitScreen", UnsignedInteger(GDK_KEY_SplitScreen));
		events_tpls.instance.setForeign("Support", UnsignedInteger(GDK_KEY_Support));
		events_tpls.instance.setForeign("TaskPane", UnsignedInteger(GDK_KEY_TaskPane));
		events_tpls.instance.setForeign("Terminal", UnsignedInteger(GDK_KEY_Terminal));
		events_tpls.instance.setForeign("Tools", UnsignedInteger(GDK_KEY_Tools));
		events_tpls.instance.setForeign("Travel", UnsignedInteger(GDK_KEY_Travel));
		events_tpls.instance.setForeign("UserPB", UnsignedInteger(GDK_KEY_UserPB));
		events_tpls.instance.setForeign("User1KB", UnsignedInteger(GDK_KEY_User1KB));
		events_tpls.instance.setForeign("User2KB", UnsignedInteger(GDK_KEY_User2KB));
		events_tpls.instance.setForeign("Video", UnsignedInteger(GDK_KEY_Video));
		events_tpls.instance.setForeign("WheelButton", UnsignedInteger(GDK_KEY_WheelButton));
		events_tpls.instance.setForeign("Word", UnsignedInteger(GDK_KEY_Word));
		events_tpls.instance.setForeign("Xfer", UnsignedInteger(GDK_KEY_Xfer));
		events_tpls.instance.setForeign("ZoomIn", UnsignedInteger(GDK_KEY_ZoomIn));
		events_tpls.instance.setForeign("ZoomOut", UnsignedInteger(GDK_KEY_ZoomOut));
		events_tpls.instance.setForeign("Away", UnsignedInteger(GDK_KEY_Away));
		events_tpls.instance.setForeign("Messenger", UnsignedInteger(GDK_KEY_Messenger));
		events_tpls.instance.setForeign("WebCam", UnsignedInteger(GDK_KEY_WebCam));
		events_tpls.instance.setForeign("MailForward", UnsignedInteger(GDK_KEY_MailForward));
		events_tpls.instance.setForeign("Pictures", UnsignedInteger(GDK_KEY_Pictures));
		events_tpls.instance.setForeign("Music", UnsignedInteger(GDK_KEY_Music));
		events_tpls.instance.setForeign("Battery", UnsignedInteger(GDK_KEY_Battery));
		events_tpls.instance.setForeign("Bluetooth", UnsignedInteger(GDK_KEY_Bluetooth));
		events_tpls.instance.setForeign("WLAN", UnsignedInteger(GDK_KEY_WLAN));
		events_tpls.instance.setForeign("UWB", UnsignedInteger(GDK_KEY_UWB));
		events_tpls.instance.setForeign("AudioForward", UnsignedInteger(GDK_KEY_AudioForward));
		events_tpls.instance.setForeign("AudioRepeat", UnsignedInteger(GDK_KEY_AudioRepeat));
		events_tpls.instance.setForeign("AudioRandomPlay", UnsignedInteger(GDK_KEY_AudioRandomPlay));
		events_tpls.instance.setForeign("Subtitle", UnsignedInteger(GDK_KEY_Subtitle));
		events_tpls.instance.setForeign("AudioCycleTrack", UnsignedInteger(GDK_KEY_AudioCycleTrack));
		events_tpls.instance.setForeign("CycleAngle", UnsignedInteger(GDK_KEY_CycleAngle));
		events_tpls.instance.setForeign("FrameBack", UnsignedInteger(GDK_KEY_FrameBack));
		events_tpls.instance.setForeign("FrameForward", UnsignedInteger(GDK_KEY_FrameForward));
		events_tpls.instance.setForeign("Time", UnsignedInteger(GDK_KEY_Time));
		events_tpls.instance.setForeign("SelectButton", UnsignedInteger(GDK_KEY_SelectButton));
		events_tpls.instance.setForeign("View", UnsignedInteger(GDK_KEY_View));
		events_tpls.instance.setForeign("TopMenu", UnsignedInteger(GDK_KEY_TopMenu));
		events_tpls.instance.setForeign("Red", UnsignedInteger(GDK_KEY_Red));
		events_tpls.instance.setForeign("Green", UnsignedInteger(GDK_KEY_Green));
		events_tpls.instance.setForeign("Yellow", UnsignedInteger(GDK_KEY_Yellow));
		events_tpls.instance.setForeign("Blue", UnsignedInteger(GDK_KEY_Blue));
		events_tpls.instance.setForeign("Suspend", UnsignedInteger(GDK_KEY_Suspend));
		events_tpls.instance.setForeign("Hibernate", UnsignedInteger(GDK_KEY_Hibernate));
		events_tpls.instance.setForeign("TouchpadToggle", UnsignedInteger(GDK_KEY_TouchpadToggle));
		events_tpls.instance.setForeign("TouchpadOn", UnsignedInteger(GDK_KEY_TouchpadOn));
		events_tpls.instance.setForeign("TouchpadOff", UnsignedInteger(GDK_KEY_TouchpadOff));
		events_tpls.instance.setForeign("AudioMicMute", UnsignedInteger(GDK_KEY_AudioMicMute));
		events_tpls.instance.setForeign("SwitchVT1", UnsignedInteger(GDK_KEY_Switch_VT_1));
		events_tpls.instance.setForeign("SwitchVT2", UnsignedInteger(GDK_KEY_Switch_VT_2));
		events_tpls.instance.setForeign("SwitchVT3", UnsignedInteger(GDK_KEY_Switch_VT_3));
		events_tpls.instance.setForeign("SwitchVT4", UnsignedInteger(GDK_KEY_Switch_VT_4));
		events_tpls.instance.setForeign("SwitchVT5", UnsignedInteger(GDK_KEY_Switch_VT_5));
		events_tpls.instance.setForeign("SwitchVT6", UnsignedInteger(GDK_KEY_Switch_VT_6));
		events_tpls.instance.setForeign("SwitchVT7", UnsignedInteger(GDK_KEY_Switch_VT_7));
		events_tpls.instance.setForeign("SwitchVT8", UnsignedInteger(GDK_KEY_Switch_VT_8));
		events_tpls.instance.setForeign("SwitchVT9", UnsignedInteger(GDK_KEY_Switch_VT_9));
		events_tpls.instance.setForeign("SwitchVT10", UnsignedInteger(GDK_KEY_Switch_VT_10));
		events_tpls.instance.setForeign("SwitchVT11", UnsignedInteger(GDK_KEY_Switch_VT_11));
		events_tpls.instance.setForeign("SwitchVT12", UnsignedInteger(GDK_KEY_Switch_VT_12));
		events_tpls.instance.setForeign("Ungrab", UnsignedInteger(GDK_KEY_Ungrab));
		events_tpls.instance.setForeign("ClearGrab", UnsignedInteger(GDK_KEY_ClearGrab));
		events_tpls.instance.setForeign("NextVMode", UnsignedInteger(GDK_KEY_Next_VMode));
		events_tpls.instance.setForeign("PrevVMode", UnsignedInteger(GDK_KEY_Prev_VMode));
		events_tpls.instance.setForeign("LogWindowTree", UnsignedInteger(GDK_KEY_LogWindowTree));
		events_tpls.instance.setForeign("LogGrabInfo", UnsignedInteger(GDK_KEY_LogGrabInfo));

		// Key modifier
		events_tpls.instance.setForeign("Control", UnsignedInteger(GDK_CONTROL_MASK));
		events_tpls.instance.setForeign("Lock",   UnsignedInteger(GDK_LOCK_MASK));
		events_tpls.instance.setForeign("Shift",   UnsignedInteger(GDK_SHIFT_MASK));
		events_tpls.instance.setForeign("Alt",   UnsignedInteger(GDK_MOD1_MASK));
		events_tpls.instance.setForeign("Meta", UnsignedInteger(GDK_META_MASK));
		events_tpls.instance.setForeign("Super", UnsignedInteger(GDK_SUPER_MASK));
		events_tpls.instance.setForeign("Hyper", UnsignedInteger(GDK_HYPER_MASK));

		EventDispatcherWrap event_dispatcher(vm, a.fifo_fd);
		vm.global_template.set("events", events_tpls.reuse(&event_dispatcher));

		// Frame wrapper
		ClassBuilder<Frame> frame_tpl(vm);

		frame_tpl.method("moveCursor", &Frame::moveCursor);
		frame_tpl.method("setStyle", &Frame::setStyle);
		frame_tpl.method("drawString", &Frame::drawString);
		frame_tpl.method("clear", &Frame::clear);
		frame_tpl.method("render", &Frame::render);
		frame_tpl.method("getRoot", &Frame::getRoot);
		frame_tpl.method("setBounds", &Frame::setBounds);
		frame_tpl.method("createSubFrame", &Frame::createSubFrame);

		frame_tpl.property("width", &Frame::getWidth);
		frame_tpl.property("height", &Frame::getHeight);
		frame_tpl.property("x", &Frame::getX);
		frame_tpl.property("y", &Frame::getY);

		vm.global_template.set("screen", frame_tpl.instantiate(frame_tpl));

		// Style constants
		ObjectTemplate consts_tpl(vm);

		consts_tpl.setForeign("normal", UnsignedInteger(A_NORMAL));
		consts_tpl.setForeign("bold", UnsignedInteger(A_BOLD));

		UnsignedInteger pair_counter = 1;
		consts_tpl.set(
			"definePair",
			function<v8::Local<v8::Value>(UnsignedInteger, UnsignedInteger)>(
				[&](UnsignedInteger fg, UnsignedInteger bg) -> v8::Local<v8::Value> {
					UnsignedInteger col = pair_counter++;

					if (init_pair(col, fg, bg) == 0)
						return Foreign<UnsignedInteger>::generate(vm, col);
					else
						return v8::Null(vm);
				}
			)
		);

		UnsignedInteger color_counter = 8;
		consts_tpl.set(
			"defineColor",
			function<v8::Local<v8::Value>(UnsignedInteger, UnsignedInteger, UnsignedInteger)>(
				[&](UnsignedInteger r, UnsignedInteger g, UnsignedInteger b) -> v8::Local<v8::Value> {
					UnsignedInteger col = color_counter++;

					if (init_color(col, r, g, b) == 0)
						return Foreign<UnsignedInteger>::generate(vm, col);
					else
						return v8::Null(vm);
				}
			)
		);

		vm.global_template.set("style", consts_tpl);

		// Logging wrapper
		ObjectTemplate log_tpl(vm);

		log_tpl.set("debug", function<void(const v8::FunctionCallbackInfo<v8::Value>&)>(
			[](const v8::FunctionCallbackInfo<v8::Value>& args) {
				ostringstream msgbuilder;

				for (int i = 0; i < args.Length(); i++) {
					if (i > 0)
						msgbuilder << ' ';

					v8::String::Utf8Value str_value(args[i]->ToString());
					msgbuilder << *str_value;
				}

				string msg = msgbuilder.str();
				logdebug(msg.c_str());
			}
		));

		log_tpl.set("info", function<void(const v8::FunctionCallbackInfo<v8::Value>&)>(
			[](const v8::FunctionCallbackInfo<v8::Value>& args) {
				ostringstream msgbuilder;

				for (int i = 0; i < args.Length(); i++) {
					if (i > 0)
						msgbuilder << ' ';

					v8::String::Utf8Value str_value(args[i]->ToString());
					msgbuilder << *str_value;
				}

				string msg = msgbuilder.str();
				loginfo(msg.c_str());
			}
		));

		log_tpl.set("warn", function<void(const v8::FunctionCallbackInfo<v8::Value>&)>(
			[](const v8::FunctionCallbackInfo<v8::Value>& args) {
				ostringstream msgbuilder;

				for (int i = 0; i < args.Length(); i++) {
					if (i > 0)
						msgbuilder << ' ';

					v8::String::Utf8Value str_value(args[i]->ToString());
					msgbuilder << *str_value;
				}

				string msg = msgbuilder.str();
				logwarn(msg.c_str());
			}
		));

		log_tpl.set("error", function<void(const v8::FunctionCallbackInfo<v8::Value>&)>(
			[](const v8::FunctionCallbackInfo<v8::Value>& args) {
				ostringstream msgbuilder;

				for (int i = 0; i < args.Length(); i++) {
					if (i > 0)
						msgbuilder << ' ';

					v8::String::Utf8Value str_value(args[i]->ToString());
					msgbuilder << *str_value;
				}

				string msg = msgbuilder.str();
				logerror(msg.c_str());
			}
		));

		vm.global_template.set("log", log_tpl);

		// Launch the JavaScript entry point
		try {
			do {
				TryCatch catcher;

				string js_entry = a.base_path + "/ext/js/entry.js";

				logdebug("Loading entry point '%s'", js_entry.c_str());
				ScriptFile script(js_entry);
				catcher.check();

				logdebug("Launching entry point");
				script.run();
				catcher.check();

				vm.modules.clear();
			} while (event_dispatcher.loop);
		} catch (Exception e) {
			logerror(e.what());
			return 1;
		}
	}

	return 0;
}
