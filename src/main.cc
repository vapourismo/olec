#include "terminal.h"
#include "app.h"
#include "js/js.h"

#include <iostream>
#include <unistd.h>
#include <fcntl.h>

using namespace std;
using namespace olec;
using namespace olec::js;

struct ApplicationWrapper: virtual Application {
	v8::Isolate* isolate;
	v8::UniquePersistent<v8::Object> event_handler;

	ApplicationWrapper():
		Application(getenv("OLEC_IPC")),
		isolate(v8::Isolate::GetCurrent())
	{
		event_handler.Reset(isolate, FunctionTemplate<void>(isolate, [](){})->GetFunction());
	}

	void reload() {
		Application::exit(1);
	}

	void exit(Integer status = 0) {
		Application::exit(status);
	}

	void main() {
		Application::main();
	}

	virtual
	void handle_event(const Event& ev) {
		if (ev.type == Event::KeyPress &&
		    ev.info.key_press.mod == GDK_CONTROL_MASK &&
		    ev.info.key_press.key == 'q') {

			exit();
			return;
		}

		v8::Local<v8::Object> eh = v8::Local<v8::Object>::New(isolate, event_handler);
		if (!eh.IsEmpty() && eh->IsCallable()) {
			eh->CallAsFunction(v8::Null(isolate), 0, nullptr);
		}
	}

	virtual
	void handle_resize(const winsize& ws) {}
};

int main(int argc, char** argv) {
	char* ipc_path = getenv("OLEC_IPC");

	if (ipc_path) {
		// Redirect cerr
		ofstream cerr_log("error.log", ios_base::app);
		cerr.rdbuf(cerr_log.rdbuf());

		int exit_status = 2;

		// Create seperate scope for all V8 tasks
		// because for some fucking reason the damn
		// thing doesn't let me exit properly.
		{
			// Initialize V8
			EngineInstance vm;
			vm.isolate->SetFatalErrorHandler([](const char* location, const char* message) {
				cerr << "JavaScript: [" << location << "]: " << message << endl;
			});

			// Application wrapper
			ClassTemplate<ApplicationWrapper> app_wrapper(vm);
			app_wrapper.method("reload", &ApplicationWrapper::reload);
			app_wrapper.method("exit", &ApplicationWrapper::exit);
			app_wrapper.method("main", &ApplicationWrapper::main);
			app_wrapper.property("eventHandler", &ApplicationWrapper::event_handler);

			// Instantiate global application object
			ApplicationWrapper app;
			vm.global_template.set("application", app_wrapper.reuse(&app));

			// Launch the JavaScript entry point
			try {
				TryCatch catcher;

				ScriptFile script("ext/js/entry.js");
				catcher.check();

				script.run();
				catcher.check();

				exit_status = app.exit_status;
			} catch (Exception e) {
				cerr << "JavaScript: " << e.what() << endl;
			} catch (Application::Error e) {
				cerr << "Application: " << e << endl;
			}
		}

		// Don't ask ...
		exit(exit_status);
		return exit_status;
	} else {
		gtk_init(&argc, &argv);

		Terminal term;

		term.spawn({argv[0]});
		term.show();

		gtk_main();
		return 0;
	}
}
