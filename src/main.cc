#include "anchor.h"
#include "terminal.h"
#include "app.h"
#include "js/js.h"

#include <iostream>
#include <cstdlib>
#include <cassert>

#include <unistd.h>
#include <fcntl.h>
#include <libgen.h>

using namespace std;
using namespace olec;
using namespace olec::js;

struct ApplicationWrapper: Application {
	v8::Isolate* isolate;
	v8::UniquePersistent<v8::Object> event_handler;

	ApplicationWrapper(const Anchor& anchor):
		Application(anchor),
		isolate(v8::Isolate::GetCurrent())
	{
		event_handler.Reset(isolate, FunctionTemplate<void>(isolate, [](){})->GetFunction());
	}

	void main() {
		dispatch();
	}

	void event(const Event& ev) {
		anchor.log(Anchor::Debug, "mod = %i, key = %i",
		           ev.info.key_press.mod, ev.info.key_press.key);

		if (ev.type == Event::KeyPress &&
		    ev.info.key_press.mod == GDK_CONTROL_MASK &&
		    ev.info.key_press.key == 'q') {

			quit();
			return;
		}

		v8::Local<v8::Object> eh = v8::Local<v8::Object>::New(isolate, event_handler);
		if (!eh.IsEmpty() && eh->IsCallable()) {
			eh->CallAsFunction(v8::Null(isolate), 0, nullptr);
		}
	}

	void resize(const winsize& ws) {

	}
};

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
			logerror("Fatal V8 Error [%s]: %s", location, message);
		});

		// V8 wrapper
		ApplicationWrapper app(a);
		ObjectTemplate app_tpl(vm);

		app_tpl.set("main", function<void()>([&app]() {
			app.main();
		}));

		vm.global_template.set("application", app_tpl);

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
			logerror("During JavaScript execution: %s", e.what());
			return 1;
		}
	}

	return 0;
}
