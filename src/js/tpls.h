#ifndef OLEC_JS_TPLS_H_
#define OLEC_JS_TPLS_H_

#include "types.h"

#include <v8.h>

namespace olec {
namespace js {

// Forward declarations
template <typename, typename...>
struct ClassTemplate;

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
};

/**
 * JavaScript Method Template
 */
template <typename T, typename R, typename... A>
struct MethodTemplate: v8::Local<v8::FunctionTemplate> {
	struct _method_wrapper {
		R (T::* method)(A...);
	};

	struct _invoke_method {
		T* instance;
		R (T::* method)(A...);

		R operator ()(A... args) {
			return (instance->*method)(args...);
		}
	};

	static
	void _method(const v8::FunctionCallbackInfo<v8::Value>& args) {
		// Check if request arguments are present
		if (check<A...>(args)) {
			// Retrieve instance pointer
			v8::Handle<v8::External> js_instance =
				v8::Handle<v8::External>::Cast(args.This()->GetInternalField(0));
			T* instance = static_cast<T*>(js_instance->Value());

			// Retrieve method offset
			v8::Handle<v8::External> js_method =
				v8::Handle<v8::External>::Cast(args.Data());
			R (T::* method)(A...) = static_cast<_method_wrapper*>(js_method->Value())->method;

			// Invoke method and generate return value
			_invoke_method im {instance, method};
			v8::Local<v8::Value> ret =
				Foreign<R>::generate(args.GetIsolate(),
				                     direct(std::function<R(A...)>(im), args));
			args.GetReturnValue().Set(ret);
		}
	}

	/**
	 * Construct a Method Template for a method.
	 */
	MethodTemplate(v8::Isolate* isolate, R (T::* method)(A...)):
		v8::Local<v8::FunctionTemplate>(v8::FunctionTemplate::New(isolate, _method,
		                                v8::External::New(isolate, new _method_wrapper {method})))
	{}
};

/**
 * JavaScript Method Template for Methods without a return value
 */
template <typename T, typename... A>
struct MethodTemplate<T, void, A...>: v8::Local<v8::FunctionTemplate> {
	struct _method_wrapper {
		void (T::* method)(A...);
	};

	struct _invoke_method {
		T* instance;
		void (T::* method)(A...);

		void operator ()(A... args) {
			(instance->*method)(args...);
		}
	};

	static
	void _method(const v8::FunctionCallbackInfo<v8::Value>& args) {
		// Check if request arguments are present
		if (check<A...>(args)) {
			// Retrieve instance pointer
			v8::Handle<v8::External> js_instance =
				v8::Handle<v8::External>::Cast(args.This()->GetInternalField(0));
			T* instance = static_cast<T*>(js_instance->Value());

			// Retrieve method offset
			v8::Handle<v8::External> js_method =
				v8::Handle<v8::External>::Cast(args.Data());
			void (T::* method)(A...) = static_cast<_method_wrapper*>(js_method->Value())->method;

			// Invoke method
			_invoke_method im {instance, method};
			direct(std::function<void(A...)>(im), args);
		}
	}

	/**
	 * Construct a Method Template for a method.
	 */
	MethodTemplate(v8::Isolate* isolate, void (T::* method)(A...)):
		v8::Local<v8::FunctionTemplate>(v8::FunctionTemplate::New(isolate, _method,
		                                v8::External::New(isolate, new _method_wrapper {method})))
	{}
};

/**
 * JavaScript Class Template with a specific constructor
 */
template <typename T, typename... A>
struct ClassTemplate: v8::Local<v8::FunctionTemplate> {
	static
	T* _invoke_constructor(A... args) {
		return new T(args...);
	}

	static
	void _destruct(const v8::WeakCallbackData<v8::External, T>& data) {
		delete data.GetParameter();
	}

	static
	void _constructor(const v8::FunctionCallbackInfo<v8::Value>& args) {
		v8::Isolate* isolate = args.GetIsolate();

		// Check argument types
		if (args.IsConstructCall() && check<A...>(args)) {
			// Redirect arguments to constructor
			T* instance = direct(std::function<T*(A...)>(_invoke_constructor), args);
			v8::Local<v8::External> self = v8::External::New(isolate, instance);

			// Construct reference and destructor
			v8::UniquePersistent<v8::External> self_p(isolate, self);
			self_p.SetWeak(instance, _destruct);

			args.This()->SetInternalField(0, self);
		}
	}

	template <typename R>
	struct _property_wrapper {
		R T::* property;
	};

	template <typename R> static
	void _getter(v8::Local<v8::String>, const v8::PropertyCallbackInfo<v8::Value>& info) {
		// Get instance pointer
		v8::Handle<v8::External> js_instance =
			v8::Handle<v8::External>::Cast(info.Holder()->GetInternalField(0));
		T* instance = static_cast<T*>(js_instance->Value());

		// Get property offset
		v8::Handle<v8::External> js_property =
			v8::Handle<v8::External>::Cast(info.Data());
		R T::* property = static_cast<_property_wrapper<R>*>(js_property->Value())->property;

		// Return the current property value
		info.GetReturnValue().Set(Foreign<R>::generate(info.GetIsolate(), instance->*property));
	}

	template <typename R> static
	void _setter(v8::Local<v8::String>, v8::Local<v8::Value> value,
	             const v8::PropertyCallbackInfo<void>& info) {
		v8::Isolate* isolate = info.GetIsolate();

		// Get instance pointer
		v8::Handle<v8::External> js_instance =
			v8::Handle<v8::External>::Cast(info.Holder()->GetInternalField(0));
		T* instance = static_cast<T*>(js_instance->Value());

		// Get property offset
		v8::Handle<v8::External> js_property =
			v8::Handle<v8::External>::Cast(info.Data());
		R T::* property = static_cast<_property_wrapper<R>*>(js_property->Value())->property;

		// Check if the new value matches the old type
		if (Foreign<R>::check(value)) {
			instance->*property = Foreign<R>::extract(value);
		} else {
			std::string exc_message = std::string("IllegalArgument: Expected ") + Foreign<R>::name;
			isolate->ThrowException(v8::String::NewFromUtf8(isolate, exc_message.c_str()));
		}
	}

	v8::Isolate* isolate;
	ObjectTemplate instance, prototype;

	/**
	 * Construct a Class Template for a class.
	 */
	ClassTemplate(v8::Isolate* isolate):
		v8::Local<v8::FunctionTemplate>(v8::FunctionTemplate::New(isolate, _constructor)),
		isolate(isolate),
		instance(isolate, (*this)->InstanceTemplate()),
		prototype(isolate, (*this)->PrototypeTemplate())
	{
		instance->SetInternalFieldCount(1);
	}

	/**
	 * Bind a method to this class.
	 */
	template <typename MR, typename... MA>
	void method(const char* name, MR (T::* method)(MA...)) {
		MethodTemplate<T, MR, MA...> method_tpl(isolate, method);
		prototype.set(name, method_tpl);
	}

	/**
	 * Bind a property accessor to this class.
	 */
	template <typename AR>
	void property(const char* name, AR T::* property) {
		instance->SetAccessor(v8::String::NewFromUtf8(isolate, name),
		                      _getter<AR>, _setter<AR>,
		                      v8::External::New(isolate, new _property_wrapper<AR> {property}));
	}
};

}
}

#endif
