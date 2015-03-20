#ifndef OLEC_JS_TYPES_H_
#define OLEC_JS_TYPES_H_

#include <cstdint>
#include <string>
#include <functional>
#include <v8.h>

namespace olec {
namespace js {

/**
 * JavaScript Number Type
 */
using Boolean = bool;

/**
 * JavaScript Number Type
 */
using Number = double;

/**
 * JavaScript Integer Type
 */
using Integer = int32_t;

/**
 * JavaScript Unsigned Integer Type
 */
using UnsignedInteger = uint32_t;

/**
 * JavaScript String Type
 */
using String = std::string;

/**
 * Prototype used to check the type of a single argument
 */
template <typename T>
struct Foreign {
	static
	bool check(const v8::Local<v8::Value>& value) {
		throw;
	}

	static
	T extract(const v8::Local<v8::Value>& value) {
		throw;
	}

	static
	v8::Local<v8::Value> generate(v8::Isolate* isolate, T value) {
		throw;
	}

	static_assert(sizeof(T) == -1, "Argument type is not supported");
};

/**
 * For boolean values
 */
template <>
struct Foreign<Boolean> {
	static
	constexpr const char* name = "Boolean";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return value->IsBoolean() || value->IsBooleanObject();
	}

	static inline
	Boolean extract(const v8::Local<v8::Value>& value) {
		return value->ToBoolean()->Value();
	}

	static inline
	v8::Local<v8::Value> generate(v8::Isolate* isolate, Boolean value) {
		return v8::Boolean::New(isolate, value);
	}
};

/**
 * For integer values
 */
template <>
struct Foreign<Integer> {
	static
	constexpr const char* name = "Integer";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return value->IsInt32();
	}

	static inline
	Integer extract(const v8::Local<v8::Value>& value) {
		return value->ToInt32()->Value();
	}

	static inline
	v8::Local<v8::Value> generate(v8::Isolate* isolate, Integer value) {
		return v8::Int32::New(isolate, value);
	}
};

/**
 * For unsigned integer values
 */
template <>
struct Foreign<UnsignedInteger> {
	static
	constexpr const char* name = "UnsignedInteger";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return value->IsUint32();
	}

	static inline
	UnsignedInteger extract(const v8::Local<v8::Value>& value) {
		return value->ToInt32()->Value();
	}

	static inline
	v8::Local<v8::Value> generate(v8::Isolate* isolate, UnsignedInteger value) {
		return v8::Uint32::New(isolate, value);
	}
};

/**
 * For number values
 */
template <>
struct Foreign<Number> {
	static
	constexpr const char* name = "Number";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return value->IsNumber() || value->IsNumberObject();
	}

	static inline
	Number extract(const v8::Local<v8::Value>& value) {
		return value->ToNumber()->Value();
	}

	static inline
	v8::Local<v8::Value> generate(v8::Isolate* isolate, Number value) {
		return v8::Number::New(isolate, value);
	}
};

/**
 * For string values
 */
template <>
struct Foreign<String> {
	static
	constexpr const char* name = "String";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return value->IsString() || value->IsStringObject();
	}

	static inline
	String extract(const v8::Local<v8::Value>& value) {
		v8::String::Utf8Value strval(value);
		return *strval;
	}
};

namespace internal {
	/**
	 * Prototype used to check for a number for arguments and their type
	 */
	template <int, typename...>
	struct ArgumentCheckN;

	/**
	 * No argument
	 */
	template <int N>
	struct ArgumentCheckN<N> {
		static inline
		bool check(const v8::FunctionCallbackInfo<v8::Value>& args) {
			return true;
		}

		template <typename R, typename F, typename... A>
		static inline
		R direct(F f, const v8::FunctionCallbackInfo<v8::Value>& args, A... rest) {
			return f(rest...);
		}
	};

	/**
	 * One argument
	 */
	template <int N, typename T>
	struct ArgumentCheckN<N, T> {
		static inline
		bool check(const v8::FunctionCallbackInfo<v8::Value>& args) {
			v8::Isolate* isolate = args.GetIsolate();

			// Check type of argument N
			if (!Foreign<T>::check(args[N])) {
				std::string exc_message =
					std::string("IllegalArgument: Expected ") + Foreign<T>::name + " as argument #" +
					std::to_string(N + 1);
				isolate->ThrowException(v8::String::NewFromUtf8(isolate, exc_message.c_str()));

				return false;
			} else {
				return true;
			}
		}

		template <typename R, typename F, typename... A>
		static inline
		R direct(F f, const v8::FunctionCallbackInfo<v8::Value>& args, A... rest) {
			return f(rest..., Foreign<T>::extract(args[N]));
		}
	};

	/**
	 * Two or more arguments
	 */
	template <int N, typename T, typename... Ts>
	struct ArgumentCheckN<N, T, Ts...> {
		static inline
		bool check(const v8::FunctionCallbackInfo<v8::Value>& args) {
			return
				ArgumentCheckN<N, T>::check(args) &&
				ArgumentCheckN<N + 1, Ts...>::check(args);
		}

		template <typename R, typename F, typename... A>
		static inline
		R direct(F f, const v8::FunctionCallbackInfo<v8::Value>& args, A... rest) {
			return ArgumentCheckN<N + 1, Ts...>::template direct<R, F>(f, args, rest...,
			                                                           Foreign<T>::extract(args[N]));
		}
	};

}

/**
 * Check for argument types.
 */
template <typename... T>
static inline
bool check(const v8::FunctionCallbackInfo<v8::Value>& args) {
	// Check is the number of present arguments matches the requested amount
	if (sizeof...(T) > (unsigned) args.Length()) {
		std::string exc_message =
			"InsufficientArguments: Expected at least " +
			std::to_string(sizeof...(T)) +
			" arguments (got " +
			std::to_string(args.Length()) +
			")";

		v8::Isolate* isolate = args.GetIsolate();
		isolate->ThrowException(v8::String::NewFromUtf8(isolate, exc_message.c_str()));

		return false;
	}

	// Check types
	return internal::ArgumentCheckN<0, T...>::check(args);
}

/**
 * Convert the arguments accoringly, and invoke a function with them.
 */
template <typename R, typename... A>
static inline
R direct(std::function<R(A...)> f, const v8::FunctionCallbackInfo<v8::Value>& args) {
	return internal::ArgumentCheckN<0, A...>::template direct<R, std::function<R(A...)>>(f, args);
}

/**
 * JavaScript Method Template
 */
template <typename T, typename R, typename... A>
struct MethodTemplate {
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

	v8::Local<v8::FunctionTemplate> value;

	/**
	 * Construct a Method Template for a method.
	 */
	MethodTemplate(v8::Isolate* isolate, R (T::* method)(A...)):
		value(v8::FunctionTemplate::New(isolate, _method,
		                                v8::External::New(isolate, new _method_wrapper {method})))
	{}
};

/**
 * JavaScript Method Template for Methods without a return value
 */
template <typename T, typename... A>
struct MethodTemplate<T, void, A...> {
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

	v8::Local<v8::FunctionTemplate> value;

	/**
	 * Construct a Method Template for a method.
	 */
	MethodTemplate(v8::Isolate* isolate, void (T::* method)(A...)):
		value(v8::FunctionTemplate::New(isolate, _method,
		                                v8::External::New(isolate, new _method_wrapper {method})))
	{}
};

/**
 * JavaScript Class Template with a specific constructor
 */
template <typename T, typename... A>
struct ClassTemplate {
	static
	T* _invoke_constructor(A... args) {
		return new T(args...);
	}

	static
	void _constructor(const v8::FunctionCallbackInfo<v8::Value>& args) {
		// Check argument types
		if (args.IsConstructCall() && check<A...>(args)) {
			// Redirect arguments to constructor
			T* instance = direct(std::function<T*(A...)>(_invoke_constructor), args);
			args.This()->SetInternalField(0, v8::External::New(args.GetIsolate(), instance));
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
	v8::Local<v8::FunctionTemplate> constructor;
	v8::Local<v8::ObjectTemplate> instance, prototype;

	/**
	 * Construct a Class Template for a class.
	 */
	ClassTemplate(v8::Isolate* isolate):
		isolate(isolate),
		constructor(v8::FunctionTemplate::New(isolate, _constructor)),
		instance(constructor->InstanceTemplate()),
		prototype(constructor->PrototypeTemplate())
	{
		instance->SetInternalFieldCount(1);
	}

	/**
	 * Set a field of the underlying prototype object.
	 */
	void set(const char* field, v8::Handle<v8::Data> value) {
		prototype->Set(v8::String::NewFromUtf8(isolate, field), value);
	}

	/**
	 * Bind a method to this class.
	 */
	template <typename MR, typename... MA>
	void method(const char* name, MR (T::* method)(MA...)) {
		MethodTemplate<T, MR, MA...> method_tpl(isolate, method);
		set(name, method_tpl.value);
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

	/* Auxiliary accessors and converters */

	inline
	v8::FunctionTemplate* operator *() {
		return *constructor;
	}

	inline
	v8::FunctionTemplate* operator ->() {
		return *constructor;
	}

	template <typename S> inline
	operator v8::Local<S>() {
		return constructor;
	}

	template <typename S> inline
	operator v8::Handle<S>() {
		return constructor;
	}
};

/**
 * JavaScript Function Template
 */
template <typename R, typename... A>
struct FunctionTemplate {
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

	v8::Local<v8::FunctionTemplate> value;

	/**
	 * Construct a Function Template
	 */
	FunctionTemplate(v8::Isolate* isolate, std::function<R(A...)> func):
		value(v8::FunctionTemplate::New(isolate, _function,
		                                v8::External::New(isolate, new std::function<R(A...)> {func})))
	{}

	/* Auxiliary accessors and converters */

	inline
	v8::FunctionTemplate* operator *() {
		return *value;
	}

	inline
	v8::FunctionTemplate* operator ->() {
		return *value;
	}

	template <typename S> inline
	operator v8::Local<S>() {
		return value;
	}

	template <typename S> inline
	operator v8::Handle<S>() {
		return value;
	}
};

/**
 * JavaScript Function Template for functions without a return value
 */
template <typename... A>
struct FunctionTemplate<void, A...> {
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

	v8::Local<v8::FunctionTemplate> value;

	/**
	 * Construct a Function Template
	 */
	FunctionTemplate(v8::Isolate* isolate, std::function<void(A...)> func):
		value(v8::FunctionTemplate::New(isolate, _function,
		                                v8::External::New(isolate, new std::function<void(A...)> {func})))
	{}

	/* Auxiliary accessors and converters */

	inline
	v8::FunctionTemplate* operator *() {
		return *value;
	}

	inline
	v8::FunctionTemplate* operator ->() {
		return *value;
	}

	template <typename S> inline
	operator v8::Local<S>() {
		return value;
	}

	template <typename S> inline
	operator v8::Handle<S>() {
		return value;
	}
};

}
}

#endif
