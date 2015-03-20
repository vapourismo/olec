#ifndef OLEC_JS_SCRIPT_H_
#define OLEC_JS_SCRIPT_H_

#include "exc.h"

#include <string>
#include <fstream>
#include <v8.h>

namespace olec {
namespace js {

/**
 * Load files as an executable script
 */
struct ScriptFile: v8::Local<v8::Script> {
	static inline
	v8::Local<v8::String> read(v8::Isolate* isolate, const char* file_path) {
		std::string contents;
		std::ifstream source(file_path);

		source.seekg(0, std::ios::end);
		contents.reserve(source.tellg());

		source.seekg(0, std::ios::beg);
		contents.assign(std::istreambuf_iterator<char>(source), std::istreambuf_iterator<char>());

		return v8::String::NewFromUtf8(isolate, contents.c_str());
	}

	static inline
	v8::Local<v8::Script> compile(v8::Isolate* isolate, const char* file_path)
		throw (Exception)
	{
		v8::Local<v8::String> contents = read(isolate, file_path);

		v8::TryCatch me;
		v8::Local<v8::Script> script =
			v8::Script::Compile(contents, v8::String::NewFromUtf8(isolate, file_path));

		if (me.HasCaught())
			throw Exception(me.Message());

		return script;
	}

	/**
	 * Load script from `file_path`
	 */
	inline
	ScriptFile(v8::Isolate* isolate, const char* file_path) throw (Exception):
		v8::Local<v8::Script>(compile(isolate, file_path))
	{}

	/**
	 * Run the script and catch errors.
	 */
	inline
	v8::Local<v8::Value> run() throw (Exception) {
		v8::TryCatch me;
		v8::Local<v8::Value> val = (*this)->Run();

		if (me.HasCaught())
			throw Exception(me.Message());

		return val;
	}
};

}
}

#endif
