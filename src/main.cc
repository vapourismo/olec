#include "anchor.h"
#include "terminal.h"
#include "app.h"
#include "js/js.h"

#include <cstdlib>
#include <iostream>
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
	Anchor a;

	if (a) {
		gtk_init(&argc, &argv);

		Terminal term(a);
		term.show();

		gtk_main();
	} else {
		// Figure where the executable is located
		char* argc_real_cstr = realpath(argv[0], nullptr);

		if (!argc_real_cstr) {
			cerr << "Failed to determine realpath of '" << argv[0] << "'" << endl;
			return 1;
		}

		string exe_dir(dirname(argc_real_cstr));
		free(argc_real_cstr);

		// Create seperate scope for all V8 tasks
		{
			// Initialize V8
			EngineInstance vm;
			vm.isolate->SetFatalErrorHandler([](const char* location, const char* message) {
				cerr << "JavaScript: [" << location << "]: " << message << endl;
			});

			// // Application wrapper
			// ClassTemplate<ApplicationWrapper, const Anchor&> app_wrapper(vm);
			// app_wrapper.method("main", &ApplicationWrapper::main);
			// app_wrapper.property("eventHandler", &ApplicationWrapper::event_handler);

			// // Instantiate global application object
			// ApplicationWrapper app(a);
			// vm.global_template.set("application", app_wrapper.reuse(&app));

			ApplicationWrapper app(a);
			ObjectTemplate app_tpl(vm);

			app_tpl.set("main", function<void()>([&app]() {
				app.main();
			}));

			vm.global_template.set("application", app_tpl);

			// Launch the JavaScript entry point
			try {
				TryCatch catcher;

				ScriptFile script(exe_dir + "/ext/js/entry.js");
				catcher.check();

				script.run();
				catcher.check();
			} catch (Exception e) {
				cerr << "JavaScript: " << e.what() << endl;
				return 1;
			}
		}
	}

	return 0;
}
