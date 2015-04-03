#include "anchor.h"
#include "terminal.h"
#include "app.h"
#include "js/js.h"

#include <iostream>
#include <cstdlib>
#include <cassert>

using namespace std;
using namespace olec;
using namespace olec::js;

struct ApplicationWrapper: Application {
	v8::Isolate* isolate;
	v8::UniquePersistent<v8::Object> key_handler;

	ApplicationWrapper(const Anchor& anchor):
		Application(anchor),
		isolate(v8::Isolate::GetCurrent())
	{
		key_handler.Reset(isolate, FunctionTemplate<void>(isolate, [](){})->GetFunction());
	}

	void event(const Event& ev) {
		if (ev.type == Event::KeyPress) {
			if (ev.info.key_press.mod == GDK_CONTROL_MASK &&
			    ev.info.key_press.key == 'q') {

				quit();
				return;
			}

			v8::Local<v8::Value> params[2] = {
				v8::Uint32::NewFromUnsigned(isolate, ev.info.key_press.mod),
				v8::Uint32::NewFromUnsigned(isolate, ev.info.key_press.key)
			};

			auto eh = v8::Local<v8::Object>::New(isolate, key_handler);
			eh->CallAsFunction(v8::Null(isolate), 2, params);
		}
	}

	void resize(const winsize& ws) {

	}

	void set_key_handler(v8::Local<v8::Object> eh) {
		if (!eh.IsEmpty() && eh->IsCallable()) {
			key_handler.Reset(isolate, eh);
		} else {
			v8::String::Utf8Value strval(eh);
			logwarn("Provided key handler '%s' is not callable", *strval);
		}
	}

	void main() {
		dispatch();
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
			logerror("[%s] %s", location, message);
		});

		// Application wrapper
		ClassBuilder<ApplicationWrapper> app_tpl(vm);
		app_tpl.method("main", &ApplicationWrapper::main);
		app_tpl.method("setKeyHandler", &ApplicationWrapper::set_key_handler);

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
		vm.global_template.set("application", app_tpl.instantiate(a));
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
