#ifndef OLEC_JS_TPLS_H_
#define OLEC_JS_TPLS_H_

#include "types.h"
#include "objtpls.h"
#include "vm.h"

namespace olec {
namespace js {

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
	void _deleter(void* instance) {
		delete static_cast<T*>(instance);
	}

	static
	T* _invoke_constructor(A... args) {
		T* instance = new T(args...);
		EngineInstance::current()->track(instance, std::function<void(void*)>(_deleter));
		return instance;
	}

	static
	void _destruct(const v8::WeakCallbackData<v8::External, v8::UniquePersistent<v8::External>>& data) {
		// Get handle
		v8::Local<v8::External> self =
			v8::Local<v8::External>::New(data.GetIsolate(), *data.GetParameter());
		T* instance = Foreign<External<T>>::extract(self);

		// Delete class instance
		EngineInstance::current()->finalize(instance);

		// Delete persistent handle
		delete data.GetParameter();
	}

	static
	void _constructor(const v8::FunctionCallbackInfo<v8::Value>& args) {
		v8::Isolate* isolate = v8::Isolate::GetCurrent();

		// Check argument types
		if (args.IsConstructCall() && check<A...>(args)) {
			// Redirect arguments to constructor
			T* instance = direct(std::function<T*(A...)>(_invoke_constructor), args);
			v8::Local<v8::External> self = v8::External::New(isolate, instance);

			// Construct reference and destructor
			auto* self_p = new v8::UniquePersistent<v8::External>(isolate, self);
			self_p->SetWeak(self_p, _destruct);

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

/**
 * JavaScript Class Template which will be instantiated in C++
 */
template <typename T>
struct ClassBuilder {
	static
	void _deleter(void* instance) {
		delete static_cast<T*>(instance);
	}

	static
	void _destruct(const v8::WeakCallbackData<v8::External, v8::UniquePersistent<v8::External>>& data) {
		// Get handle
		v8::Local<v8::External> self =
			v8::Local<v8::External>::New(data.GetIsolate(), *data.GetParameter());
		T* instance = Foreign<External<T>>::extract(self);

		// Delete class instance
		EngineInstance::current()->finalize(instance);

		// Delete persistent handle
		delete data.GetParameter();
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
	ObjectTemplate instance;

	/**
	 * Construct a Class Template for a class.
	 */
	ClassBuilder(v8::Isolate* isolate):
		isolate(isolate),
		instance(isolate)
	{
		instance->SetInternalFieldCount(1);
	}

	/**
	 * Bind a method to this class.
	 */
	template <typename MR, typename... MA>
	void method(const char* name, MR (T::* method)(MA...)) {
		MethodTemplate<T, MR, MA...> method_tpl(isolate, method);
		instance.set(name, method_tpl);
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

	/**
	 * Instantiate this class.
	 */
	template <typename... A>
	v8::Local<v8::Object> instantiate(A&&... args) {
		return reuse(new T(std::forward<A>(args)...));
	}

	/**
	 * Use an existing instance of this class.
	 */
	v8::Local<v8::Object> reuse(T* me) {
		// Construct C++ type
		v8::Local<v8::External> self = v8::External::New(isolate, me);

		// Instantiate object
		v8::Local<v8::Object> obj = instance->NewInstance();
		obj->SetInternalField(0, self);

		return obj;
	}
};

}
}

#endif
