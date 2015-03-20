// #include "terminal.h"
// #include "app.h"

// #include <iostream>
// #include <unistd.h>
// #include <fcntl.h>

// using namespace std;
// using namespace olec;

// int main(int argc, char** argv) {
// 	char* ipc_path = getenv("OLEC_IPC");

// 	if (ipc_path) {
// 		Application app(ipc_path);

// 		app.key_map.bind(GDK_CONTROL_MASK, GDK_KEY_q, [&app](KeyModifier mod, KeySymbol key) {
// 			app.exit();
// 			return true;
// 		});

// 		return app.main();
// 	} else {
// 		gtk_init(&argc, &argv);

// 		Terminal term;

// 		term.show();
// 		term.spawn({argv[0]});

// 		gtk_main();
// 	}

// 	return 0;
// }

#include <iostream>
#include <string>
#include <tuple>
#include <v8.h>

using namespace std;
using namespace v8;

struct JavaScriptVM {
	v8::Isolate* isolate;

	JavaScriptVM();

	~JavaScriptVM();

	inline
	operator v8::Isolate*() {
		return isolate;
	}
};

JavaScriptVM::JavaScriptVM() {
	V8::InitializeICU();
	V8::Initialize();

	isolate = Isolate::New();
}

JavaScriptVM::~JavaScriptVM() {
	isolate->Dispose();
	V8::Dispose();
}

static
JavaScriptVM jsvm;

struct MyObject {
	MyObject(bool b, int32_t i, double d) {
		cout << b << endl << i << endl << d << endl;
	}
};

bool js_check_param_length(const FunctionCallbackInfo<Value>& args, int n) {
	if (n > args.Length()) {
		string exc_message = "InsufficientArguments: Expected at least " + to_string(n) + " arguments (got " + to_string(args.Length()) + ")";

		Isolate* isolate = args.GetIsolate();
		isolate->ThrowException(String::NewFromUtf8(isolate, exc_message.c_str()));

		return false;
	}

	return true;
}

template <typename>
struct ArgumentCheckSingle;

template <>
struct ArgumentCheckSingle<bool> {
	static
	bool check(Isolate* isolate, int n, const Local<Value>& value) {
		if (!value->IsBoolean()) {
			string exc_message = "IllegalArgument: Expected Boolean as argument #" + to_string(n + 1);
			isolate->ThrowException(String::NewFromUtf8(isolate, exc_message.c_str()));

			return false;
		}

		return true;
	}

	static
	bool extract(const Local<Value>& value) {
		return value->ToBoolean()->Value();
	}
};

template <>
struct ArgumentCheckSingle<int32_t> {
	static
	bool check(Isolate* isolate, int n, const Local<Value>& value) {
		if (!value->IsInt32()) {
			string exc_message = "IllegalArgument: Expected Int32 as argument #" + to_string(n + 1);
			isolate->ThrowException(String::NewFromUtf8(isolate, exc_message.c_str()));

			return false;
		}

		return true;
	}

	static
	int32_t extract(const Local<Value>& value) {
		return value->ToInt32()->Value();
	}
};

template <>
struct ArgumentCheckSingle<double> {
	static
	bool check(Isolate* isolate, int n, const Local<Value>& value) {
		if (!value->IsNumber()) {
			string exc_message = "IllegalArgument: Expected Int32 as argument #" + to_string(n + 1);
			isolate->ThrowException(String::NewFromUtf8(isolate, exc_message.c_str()));

			return false;
		}

		return true;
	}

	static
	double extract(const Local<Value>& value) {
		return value->ToNumber()->Value();
	}
};

template <int, typename...>
struct ArgumentCheckN;

template <int N>
struct ArgumentCheckN<N> {
	static inline
	bool check(const FunctionCallbackInfo<Value>& args) {
		return true;
	}
};

template <int N, typename T>
struct ArgumentCheckN<N, T> {
	static inline
	bool check(const FunctionCallbackInfo<Value>& args) {
		return ArgumentCheckSingle<T>::check(args.GetIsolate(), N, args[N]);
	}

	template <typename R, typename F, typename... A>
	static inline
	R direct(F f, const FunctionCallbackInfo<Value>& args, A... rest) {
		return f(rest..., ArgumentCheckSingle<T>::extract(args[N]));
	}
};

template <int N, typename T, typename... Ts>
struct ArgumentCheckN<N, T, Ts...> {
	static inline
	bool check(const FunctionCallbackInfo<Value>& args) {
		return
			ArgumentCheckN<N, T>::check(args) &&
			ArgumentCheckN<N + 1, Ts...>::check(args);
	}

	template <typename R, typename F, typename... A>
	static inline
	R direct(F f, const FunctionCallbackInfo<Value>& args, A... rest) {
		return ArgumentCheckN<N + 1, Ts...>::template direct<R, F>(f, args, rest...,
		                                                           ArgumentCheckSingle<T>::extract(args[N]));
	}
};

template <typename... Ts>
using ArgumentCheck = ArgumentCheckN<0, Ts...>;

template <typename T, typename... A>
T* construct(A... args) {
	return new T(args...);
}

template <typename T, typename... Params>
void js_constructor(const FunctionCallbackInfo<Value>& args) {
	if (!js_check_param_length(args, sizeof...(Params)))
		return;

	ArgumentCheck<Params...>::check(args);

	T* instance = ArgumentCheck<Params...>::template direct<T*>(construct<T, Params...>, args);

	args.GetReturnValue().Set(External::New(jsvm, instance));
}

int main() {
	Isolate::Scope isolate_scope(jsvm);
	HandleScope handle_scope(jsvm);

	Local<ObjectTemplate> global = ObjectTemplate::New();

	Local<ObjectTemplate> class_tpl = ObjectTemplate::New(jsvm);
	class_tpl->SetCallAsFunctionHandler(js_constructor<MyObject, bool, int32_t, double>);

	global->Set(String::NewFromUtf8(jsvm, "Test"),
	            class_tpl);

	Local<Context> context = Context::New(jsvm, nullptr, global);
	Context::Scope context_scope(context);

	Local<Script> script = Script::Compile(String::NewFromUtf8(jsvm, "new Test(true, 1337, 11273091.23)"));
	Local<Value> result = script->Run();

	String::Utf8Value utf8(result);
	cout << *utf8 << endl;

	return 0;
}
