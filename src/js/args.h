#ifndef OLEC_JS_ARGS_H_
#define OLEC_JS_ARGS_H_

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
 * JavaScript String Type
 */
using String = std::string;

namespace internal {
	/**
	 * Prototype used to check the type of a single argument
	 */
	template <typename>
	struct ArgumentCheckSingle;

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
			if (!value->IsNumber() && !value->IsNumberObject()) {
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

	/**
	 * Type constructor function
	 */
	template <typename T, typename... A>
	T* construct_type(A... args) {
		return new T(args...);
	}
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
 * JavaScript function for a type constructor
 */
template <typename T, typename... Params>
void ctor(const v8::FunctionCallbackInfo<v8::Value>& args) {
	if (check<Params...>(args)) {
		T* instance = direct(std::function<T*(Params...)>(internal::construct_type<T, Params...>), args);
		args.GetReturnValue().Set(v8::External::New(args.GetIsolate(), instance));
	}
}

}
}

#endif
