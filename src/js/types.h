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
using String = const char*;

namespace internal {
	/**
	 * Prototype used to check the type of a single argument
	 */
	template <typename T>
	struct ArgumentCheckSingle {
		static
		bool check(v8::Isolate* isolate, int n, const v8::Local<v8::Value>& value) {
			throw;
		}

		static
		T extract(const v8::Local<v8::Value>& value) {
			throw;
		}

		static_assert(sizeof(T) == -1, "Argument type is not supported");
	};

	/**
	 * For boolean arguments
	 */
	template <>
	struct ArgumentCheckSingle<Boolean> {
		static
		bool check(v8::Isolate* isolate, int n, const v8::Local<v8::Value>& value) {
			if (!value->IsBoolean() && !value->IsBooleanObject()) {
				std::string exc_message =
					"IllegalArgument: Expected Boolean as argument #" +
					std::to_string(n + 1);
				isolate->ThrowException(v8::String::NewFromUtf8(isolate, exc_message.c_str()));

				return false;
			}

			return true;
		}

		static
		Boolean extract(const v8::Local<v8::Value>& value) {
			return value->ToBoolean()->Value();
		}
	};

	/**
	 * For integer arguments
	 */
	template <>
	struct ArgumentCheckSingle<Integer> {
		static
		bool check(v8::Isolate* isolate, int n, const v8::Local<v8::Value>& value) {
			if (!value->IsInt32()) {
				std::string exc_message =
					"IllegalArgument: Expected Integer as argument #" +
					std::to_string(n + 1);
				isolate->ThrowException(v8::String::NewFromUtf8(isolate, exc_message.c_str()));

				return false;
			}

			return true;
		}

		static
		Integer extract(const v8::Local<v8::Value>& value) {
			return value->ToInt32()->Value();
		}
	};


	/**
	 * For unsigned integer arguments
	 */
	template <>
	struct ArgumentCheckSingle<UnsignedInteger> {
		static
		bool check(v8::Isolate* isolate, int n, const v8::Local<v8::Value>& value) {
			if (!value->IsUint32()) {
				std::string exc_message =
					"IllegalArgument: Expected UnsignedInteger as argument #" +
					std::to_string(n + 1);
				isolate->ThrowException(v8::String::NewFromUtf8(isolate, exc_message.c_str()));

				return false;
			}

			return true;
		}

		static
		UnsignedInteger extract(const v8::Local<v8::Value>& value) {
			return value->ToInt32()->Value();
		}
	};

	/**
	 * For number arguments
	 */
	template <>
	struct ArgumentCheckSingle<Number> {
		static
		bool check(v8::Isolate* isolate, int n, const v8::Local<v8::Value>& value) {
			if (!value->IsNumber() && !value->IsNumberObject()) {
				std::string exc_message =
					"IllegalArgument: Expected Number as argument #" +
					std::to_string(n + 1);
				isolate->ThrowException(v8::String::NewFromUtf8(isolate, exc_message.c_str()));

				return false;
			}

			return true;
		}

		static
		Number extract(const v8::Local<v8::Value>& value) {
			return value->ToNumber()->Value();
		}
	};

	/**
	 * For string arguments
	 */
	template <>
	struct ArgumentCheckSingle<String> {
		static
		bool check(v8::Isolate* isolate, int n, const v8::Local<v8::Value>& value) {
			if (!value->IsString() && !value->IsStringObject()) {
				std::string exc_message =
					"IllegalArgument: Expected String as argument #" +
					std::to_string(n + 1);
				isolate->ThrowException(v8::String::NewFromUtf8(isolate, exc_message.c_str()));

				return false;
			}

			return true;
		}

		static
		String extract(const v8::Local<v8::Value>& value) {
			v8::String::Utf8Value strval(value);
			return *strval;
		}
	};

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
			return ArgumentCheckSingle<T>::check(args.GetIsolate(), N, args[N]);
		}

		template <typename R, typename F, typename... A>
		static inline
		R direct(F f, const v8::FunctionCallbackInfo<v8::Value>& args, A... rest) {
			return f(rest..., ArgumentCheckSingle<T>::extract(args[N]));
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
			                                                           ArgumentCheckSingle<T>::extract(args[N]));
		}
	};

}

/**
 * Check for argument types.
 */
template <typename... T>
static inline
bool check(const v8::FunctionCallbackInfo<v8::Value>& args) {
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
		if (js::check<A...>(args)) {
			v8::Handle<v8::External> js_instance =
				v8::Handle<v8::External>::Cast(args.This()->GetInternalField(0));
			v8::Handle<v8::External> js_method =
				v8::Handle<v8::External>::Cast(args.Data());

			T* instance = static_cast<T*>(js_instance->Value());
			R (T::* method)(A...) = static_cast<_method_wrapper*>(js_method->Value())->method;

			_invoke_method im {instance, method};
			js::direct(std::function<R(A...)>(im), args);
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
		if (args.IsConstructCall() && js::check<A...>(args)) {
			T* instance = js::direct(std::function<T*(A...)>(_invoke_constructor), args);
			args.This()->SetInternalField(0, v8::External::New(args.GetIsolate(), instance));
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
	void bind(const char* field, v8::Handle<v8::Data> value) {
		prototype->Set(v8::String::NewFromUtf8(isolate, field), value);
	}

	/**
	 * Bind a method to this class.
	 */
	template <typename MR, typename... MA>
	void method(const char* name, MR (T::* method)(MA...)) {
		MethodTemplate<T, MR, MA...> method_tpl(isolate, method);
		bind(name, method_tpl);
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

}
}

#endif
