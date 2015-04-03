#ifndef OLEC_JS_OBJTPL_H_
#define OLEC_JS_OBJTPL_H_

#include "types.h"

#include <functional>
#include <v8.h>

namespace olec {
namespace js {

/**
 * JavaScript Function Template
 */
template <typename R, typename... A>
struct FunctionTemplate: v8::Local<v8::FunctionTemplate> {
	static
	void _function(const v8::FunctionCallbackInfo<v8::Value>& args) {
		// Check if request arguments are present
		if (check<A...>(args)) {
			// Retrieve function
			v8::Handle<v8::External> js_method =
				v8::Handle<v8::External>::Cast(args.Data());
			std::function<R(A...)>* func =
				static_cast<std::function<R(A...)>*>(js_method->Value());

			// Invoke function and generate return value
			v8::Local<v8::Value> ret =
				Foreign<R>::generate(args.GetIsolate(),
				                     direct(*func, args));
			args.GetReturnValue().Set(ret);
		}
	}

	/**
	 * Construct a Function Template
	 */
	FunctionTemplate(v8::Isolate* isolate, std::function<R(A...)> func):
		v8::Local<v8::FunctionTemplate>(
			v8::FunctionTemplate::New(
				isolate, _function,
				v8::External::New(isolate, new std::function<R(A...)> {func})
			)
		)
	{}
};

/**
 * JavaScript Function Template for functions without a return value
 */
template <typename... A>
struct FunctionTemplate<void, A...>: v8::Local<v8::FunctionTemplate> {
	static
	void _function(const v8::FunctionCallbackInfo<v8::Value>& args) {
		// Check if request arguments are present
		if (check<A...>(args)) {
			// Retrieve function
			v8::Handle<v8::External> js_method =
				v8::Handle<v8::External>::Cast(args.Data());
			std::function<void(A...)>* func =
				static_cast<std::function<void(A...)>*>(js_method->Value());

			// Invoke function
			direct(*func, args);
		}
	}

	/**
	 * Construct a Function Template
	 */
	FunctionTemplate(v8::Isolate* isolate, std::function<void(A...)> func):
		v8::Local<v8::FunctionTemplate>(
			v8::FunctionTemplate::New(
				isolate, _function,
				v8::External::New(isolate, new std::function<void(A...)> {func})
			)
		)
	{}
};

template <typename T, typename... A>
struct ClassTemplate;

/**
 * JavaScript Object Template
 */
struct ObjectTemplate: v8::Local<v8::ObjectTemplate> {
	v8::Isolate* isolate;

	inline
	ObjectTemplate(v8::Isolate* isolate):
		v8::Local<v8::ObjectTemplate>(v8::ObjectTemplate::New()),
		isolate(isolate)
	{}

	inline
	ObjectTemplate(v8::Isolate* isolate, v8::Local<v8::ObjectTemplate> value):
		v8::Local<v8::ObjectTemplate>(value),
		isolate(isolate)
	{}

	template <typename T, typename... A>
	inline
	void set(const char* name, ClassTemplate<T, A...>& class_tpl) {
		v8::Local<v8::String> name_val = v8::String::NewFromUtf8(isolate, name);
		class_tpl->SetClassName(name_val);
		(*this)->Set(name_val, class_tpl);
	}

	inline
	void set(const char* name, v8::Handle<v8::Data> data) {
		(*this)->Set(v8::String::NewFromUtf8(isolate, name), data);
	}

	template <typename R, typename... A> inline
	void set(const char* name, std::function<R(A...)> func) {
		FunctionTemplate<R, A...> func_tpl(isolate, func);
		set(name, func_tpl);
	}

	template <typename R, typename... A> inline
	void set(const char* name, R (* func)(A...)) {
		FunctionTemplate<R, A...> func_tpl(isolate, func);
		set(name, func_tpl);
	}

	template <typename T> inline
	void setForeign(const char* name, T&& val) {
		set(name, Foreign<T>::generate(isolate, std::forward<T>(val)));
	}
};

}
}

#endif
