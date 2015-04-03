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
 * JavaScript External Type
 */
template <typename T>
using External = T*;

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
	v8::Local<v8::Boolean> generate(v8::Isolate* isolate, Boolean value) {
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
	v8::Local<v8::Integer> generate(v8::Isolate* isolate, Integer value) {
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
	v8::Local<v8::Integer> generate(v8::Isolate* isolate, UnsignedInteger value) {
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
	v8::Local<v8::Number> generate(v8::Isolate* isolate, Number value) {
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
		return String(*strval ? *strval : "<none>");
	}

	static inline
	v8::Local<v8::String> generate(v8::Isolate* isolate, String value) {
		return v8::String::NewFromUtf8(isolate, value.c_str());
	}
};

/**
 * For external values
 */
template <typename T>
struct Foreign<External<T>> {
	static
	constexpr const char* name = "External";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return value->IsExternal();
	}

	static inline
	External<T> extract(const v8::Local<v8::Value>& value) {
		v8::Handle<v8::External> js_value = v8::Handle<v8::External>::Cast(value);
		return static_cast<External<T>>(js_value->Value());;
	}

	static inline
	v8::Local<v8::External> generate(v8::Isolate* isolate, External<T> value) {
		return v8::External::New(isolate, value);
	}
};

/**
 * For other values
 */
template <>
struct Foreign<v8::UniquePersistent<v8::Object>> {
	static
	constexpr const char* name = "Object";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return value->IsObject();
	}

	static inline
	v8::UniquePersistent<v8::Object> extract(const v8::Local<v8::Value>& value) {
		return v8::UniquePersistent<v8::Object>(v8::Isolate::GetCurrent(), value->ToObject());
	}

	static inline
	v8::Local<v8::Object> generate(v8::Isolate* isolate, v8::UniquePersistent<v8::Object>& value) {
		return v8::Local<v8::Object>::New(isolate, value);
	}
};

/**
 * For other values
 */
template <>
struct Foreign<v8::UniquePersistent<v8::Value>> {
	static
	constexpr const char* name = "Value";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return true;
	}

	static inline
	v8::UniquePersistent<v8::Value> extract(v8::Local<v8::Value> value) {
		return v8::UniquePersistent<v8::Value>(v8::Isolate::GetCurrent(), value);
	}

	static inline
	v8::Local<v8::Value> generate(v8::Isolate* isolate, v8::UniquePersistent<v8::Value>& value) {
		return v8::Local<v8::Value>::New(isolate, value);
	}
};

/**
 * For raw values
 */
template <>
struct Foreign<v8::Local<v8::Object>> {
	static
	constexpr const char* name = "Object";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return value->IsObject();
	}

	static inline
	v8::Local<v8::Object> extract(v8::Local<v8::Value> value) {
		return value->ToObject();
	}

	static inline
	v8::Local<v8::Object> generate(v8::Isolate* isolate, v8::Local<v8::Object> value) {
		return value;
	}
};

/**
 * For raw values
 */
template <>
struct Foreign<v8::Local<v8::Value>> {
	static
	constexpr const char* name = "Value";

	static inline
	bool check(const v8::Local<v8::Value>& value) {
		return true;
	}

	static inline
	v8::Local<v8::Value> extract(v8::Local<v8::Value> value) {
		return value;
	}

	static inline
	v8::Local<v8::Value> generate(v8::Isolate* isolate, v8::Local<v8::Value> value) {
		return value;
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
		R direct(F f, const v8::FunctionCallbackInfo<v8::Value>& args, A&&... rest) {
			return f(std::forward<A>(rest)...);
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
		R direct(F f, const v8::FunctionCallbackInfo<v8::Value>& args, A&&... rest) {
			return f(std::forward<A>(rest)..., Foreign<T>::extract(args[N]));
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
		R direct(F f, const v8::FunctionCallbackInfo<v8::Value>& args, A&&... rest) {
			return ArgumentCheckN<N + 1, Ts...>::template direct<R, F>(f, args, std::forward<A>(rest)...,
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
	return internal::ArgumentCheckN<0, A...>::template direct<R>(f, args);
}

}
}

#endif
