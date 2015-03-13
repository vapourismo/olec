#include "olec.h"
#include "curses.h"

#include <signal.h>
#include <unistd.h>
#include <termios.h>
#include <sys/ioctl.h>

static
bool olec_kb_quit(Olec* olec, OlecKeyModifier mod, OlecKeySymbol key) {
	olec->exit_status = OLEC_CHILD_EXIT_OK;
	event_base_loopbreak(olec->event_base);

	return true;
}

static
bool olec_kb_reload(Olec* olec, OlecKeyModifier mod, OlecKeySymbol key) {
	olec->exit_status = OLEC_CHILD_EXIT_RELOAD;
	event_base_loopbreak(olec->event_base);

	return true;
}

static
void olec_read_fd(int event_fd, short what, Olec* olec) {
	OlecEvent event;

	if (!olec_event_read(event_fd, &event)) {
		olec->exit_status = OLEC_CHILD_EXIT_ERROR;
		event_base_loopbreak(olec->event_base);
		return;
	}

	// On key press event
	if (event.type == OLEC_KEY_PRESS)
		olec_key_map_invoke(&olec->global_keymap, event.info.key_press.mod,
		                    event.info.key_press.key);

	// Render main frame
	olec_main_frame_render(&olec->main_frame);
}

static
void olec_handle_resize(int event_fd, short what, Olec* olec) {
	// Resize terminal
	struct winsize ws;
	if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) == 0)
		resizeterm(ws.ws_row, ws.ws_col);

	// Call hooks
	olec_main_frame_update(&olec->main_frame);
	olec_main_frame_render(&olec->main_frame);
}

static
void olec_handle_reload(int event_fd, short what, Olec* olec) {
	olec->exit_status = OLEC_CHILD_EXIT_RELOAD;
	event_base_loopbreak(olec->event_base);
}

bool olec_init(Olec* olec, const char* ipc_path) {
	olec->exit_status = OLEC_CHILD_EXIT_OK;

	olec->event_base = event_base_new();
	if (!olec->event_base)
		return false;

	olec->event_fd = open(ipc_path, O_RDONLY | O_NONBLOCK, 0);
	if (olec->event_fd < 0)
		return false;

	// Setup keymap
	olec_key_map_init(&olec->global_keymap);
	olec_key_map_bind(&olec->global_keymap, GDK_CONTROL_MASK, GDK_KEY_q,
	                  (OlecKeyHook) olec_kb_quit, olec);
	olec_key_map_bind(&olec->global_keymap, GDK_CONTROL_MASK, GDK_KEY_r,
	                  (OlecKeyHook) olec_kb_reload, olec);

	// Initialize screen
	initscr();
	noecho();
	raw();

	// Colors
	start_color();
	init_pair(1, COLOR_BLUE, COLOR_WHITE);

	// Init main frame
	olec_main_frame_init(&olec->main_frame);

	return true;
}

int olec_main(Olec* olec) {
	olec_main_frame_render(&olec->main_frame);

	// Create events
	struct event* input_event = event_new(olec->event_base, olec->event_fd, EV_PERSIST | EV_READ,
	                                      (event_callback_fn) olec_read_fd, olec);
	struct event* winch_event = event_new(olec->event_base, SIGWINCH, EV_PERSIST | EV_SIGNAL,
	                                      (event_callback_fn) olec_handle_resize, olec);
	struct event* reload_event = event_new(olec->event_base, SIGUSR1, EV_PERSIST | EV_SIGNAL,
	                                       (event_callback_fn) olec_handle_reload, olec);

	// Main loop
	if (input_event) {
		event_add(input_event, NULL);
		event_add(winch_event, NULL);
		event_add(reload_event, NULL);

		event_base_dispatch(olec->event_base);

		event_del(input_event);
		event_del(winch_event);
		event_del(reload_event);

		event_free(input_event);
		event_free(winch_event);
		event_free(reload_event);
	}

	// Clean resources
	endwin();
	event_base_free(olec->event_base);
	close(olec->event_fd);

	return olec->exit_status;
}
