#ifndef OLEC_JS_SCRIPT_H_
#define OLEC_JS_SCRIPT_H_

#include <string>
#include <fstream>
#include <v8.h>

namespace olec {
namespace js {

/**
 * Load files as an executable script
 */
struct ScriptFile {
	v8::Local<v8::Script> script;

	/**
	 * Load script from `file_path`
	 */
	inline
	ScriptFile(v8::Isolate* isolate, const char* file_path) {
		std::string contents;
		std::ifstream source(file_path);

		source.seekg(0, std::ios::end);
		contents.reserve(source.tellg());

		source.seekg(0, std::ios::beg);
		contents.assign(std::istreambuf_iterator<char>(source), std::istreambuf_iterator<char>());

		script = v8::Script::Compile(v8::String::NewFromUtf8(isolate, contents.c_str()),
		                             v8::String::NewFromUtf8(isolate, file_path));
	}

	/* Auxiliary accessors and converters */

	inline
	v8::Script* operator *() {
		return *script;
	}

	inline
	v8::Script* operator ->() {
		return *script;
	}

	template <typename S> inline
	operator v8::Local<S>() {
		return script;
	}

	template <typename S> inline
	operator v8::Handle<S>() {
		return script;
	}
};

}
}

#endif
