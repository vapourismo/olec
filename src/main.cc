#include "anchor.h"
#include "terminal.h"
#include "events.h"
#include "js/js.h"

#include <iostream>
#include <cstdlib>
#include <cassert>

using namespace std;
using namespace olec;
using namespace olec::js;

int main(int argc, char** argv) {
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

		// Event dispatcher
		// ClassBuilder<EventDispatcher> event_tpls(vm);
		// event_tpls.method("dispatch", &EventDispatcher::dispatch);
		// event_tpls.method("setKeyHandler", &EventDispatcher::set_key_handler);

		// Logging wrapper
		ObjectTemplate log_tpl(vm);

		log_tpl.set("debug", function<void(String)>([](String msg) {
			logdebug(msg.c_str());
		}));

		log_tpl.set("info", function<void(String)>([](String msg) {
			loginfo(msg.c_str());
		}));

		log_tpl.set("warn", function<void(String)>([](String msg) {
			logwarn(msg.c_str());
		}));

		log_tpl.set("error", function<void(String)>([](String msg) {
			logerror(msg.c_str());
		}));

		// Submit object templates
		// vm.global_template.set("event", event_tpls.instantiate(a.fifo_fd, vm));
		vm.global_template.set("log", log_tpl);

		// Launch the JavaScript entry point
		try {
			TryCatch catcher;

			string js_entry = a.base_path + "/ext/js/entry.js";

			logdebug("Loading entry point '%s'", js_entry.c_str());
			ScriptFile script(js_entry);
			catcher.check();

			logdebug("Launching entry point");
			script.run();
			catcher.check();
		} catch (Exception e) {
			logerror(e.what());
			return 1;
		}
	}

	return 0;
}
