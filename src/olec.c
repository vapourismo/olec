#include "olec.h"

#include <signal.h>
#include <unistd.h>

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

	if (event.type == OLEC_KEY_PRESS) {
		olec_key_map_invoke(&olec->global_keymap, event.info.key_press.mod,
		                    event.info.key_press.key);
	}
}

static
void olec_handle_resize(int event_fd, short what, Olec* olec) {
	// Resize event
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

	return true;
}

int olec_main(Olec* olec) {
	// Initialize screen
	initscr();
	noecho();
	raw();
	start_color();

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
