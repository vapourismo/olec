#ifndef OLEC_JS_EXC_H_
#define OLEC_JS_EXC_H_

#include <exception>
#include <string>
#include <v8.h>

namespace olec {
namespace js {

/**
 * Exception
 */
struct Exception: virtual std::exception {
	int line, column_start, column_end;
	std::string line_contents, file_path, message;

	std::string what_msg;

	inline
	Exception(v8::Local<v8::Message> msg) {
		v8::String::Utf8Value u_source_line(msg->GetSourceLine());
		v8::String::Utf8Value u_source_file(msg->GetScriptResourceName());
		v8::String::Utf8Value u_message(msg->Get());

		line = msg->GetLineNumber();
		column_start = msg->GetStartColumn();
		column_end = msg->GetEndColumn();
		line_contents = *u_source_line;
		file_path = *u_source_file;
		message = *u_message;

		what_msg = "[" + file_path + ":" + std::to_string(line) + ":" + std::to_string(column_start + 1) + "] " + message;
	}

	virtual
	const char* what() const noexcept {
		return what_msg.c_str();
	}
};

/**
 *
 */
struct TryCatch: v8::TryCatch {
	inline
	void check() throw (js::Exception) {
		if (HasCaught()) {
		    throw js::Exception(Message());
		}
	}
};

}
}

#endif
