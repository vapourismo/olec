#include "anchor.h"
#include "terminal.h"
#include "events.h"
#include "js/js.h"

#include <iostream>
#include <sstream>
#include <cstdlib>
#include <cassert>

#include <ncurses.h>

using namespace std;
using namespace olec;
using namespace olec::js;

struct EventDispatcherWrap: EventDispatcher {
	v8::Isolate* isolate;
	v8::UniquePersistent<v8::Object> key_handler;

	bool loop = false;

	EventDispatcherWrap(v8::Isolate* isolate, int fd):
		EventDispatcher(fd),
		isolate(isolate)
	{
		key_handler.Reset(isolate, FunctionTemplate<void>(isolate, [](){})->GetFunction());
	}

	void dispatch() {
		EventDispatcher::dispatch();
	}

	void quit() {
		logdebug("Quit event dispatcher");
		EventDispatcher::quit();
		loop = false;
	}

	void reload() {
		logdebug("Reload application");
		EventDispatcher::quit();
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

	v8::Local<v8::Value> createSubFrame(UnsignedInteger x, UnsignedInteger y,
	                                    UnsignedInteger w, UnsignedInteger h) {
		NCursesWindow* sub_screen = subwin(screen, h, w, y, x);

		if (sub_screen)
			return type_template.instantiate(type_template, sub_screen);
		else
			return v8::Null(v8::Isolate::GetCurrent());
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
		ClassBuilder<EventDispatcherWrap> event_tpls(vm);

		event_tpls.method("dispatch", &EventDispatcherWrap::dispatch);
		event_tpls.method("quit", &EventDispatcherWrap::quit);
		event_tpls.method("reload", &EventDispatcherWrap::reload);
		event_tpls.property("keyHandler",
		                    &EventDispatcherWrap::getKeyHandler,
		                    &EventDispatcherWrap::setKeyHandler);

		ObjectTemplate event_keys_tpls(vm);

		event_keys_tpls.setForeign("control", UnsignedInteger(GDK_CONTROL_MASK));
		event_keys_tpls.setForeign("q", UnsignedInteger('q'));
		event_keys_tpls.setForeign("r", UnsignedInteger('r'));

		event_tpls.instance.set("keys", event_keys_tpls);

		EventDispatcherWrap event_dispatcher(vm, a.fifo_fd);
		vm.global_template.set("event", event_tpls.reuse(&event_dispatcher));

		// Frame wrapper
		ClassBuilder<Frame> frame_tpl(vm);

		frame_tpl.method("moveCursor", &Frame::moveCursor);
		frame_tpl.method("setStyle", &Frame::setStyle);
		frame_tpl.method("drawString", &Frame::drawString);
		frame_tpl.method("clear", &Frame::clear);
		frame_tpl.method("render", &Frame::render);
		frame_tpl.method("createSubFrame", &Frame::createSubFrame);
		frame_tpl.property("width", &Frame::getWidth);
		frame_tpl.property("height", &Frame::getHeight);

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
