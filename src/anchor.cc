#include "anchor.h"

#include <cassert>
#include <cstdarg>

#include <pty.h>
#include <libgen.h>
#include <fcntl.h>
#include <sys/stat.h>

using namespace std;

namespace olec {

const Anchor* Anchor::self = nullptr;

Anchor::Anchor(const char* progname):
	fifo_path("/tmp/olec-" + to_string(getpid()))
{
	assert(mkfifo(fifo_path.c_str(), S_IWUSR | S_IRUSR) == 0);

	// Fork child process with new pseudo terminal
	pid = forkpty(&pty_fd, nullptr, nullptr, nullptr);
	assert(pid >= 0);

	if (pid == 0) {
		// Child process
		fifo_fd = open(fifo_path.c_str(), O_RDONLY | O_NONBLOCK);
		assert(fifo_fd > 0);
	} else if (pid > 0) {
		// Parent process
		fifo_fd = open(fifo_path.c_str(), O_WRONLY);
		assert(fifo_fd > 0);
	}

	// Logging
	char* env_home = getenv("HOME");
	string log_path = env_home ? (string(env_home) + "/.olec.log") : "olec.log";

	log_fd = fopen(log_path.c_str(), "a+");
	assert(log_fd != nullptr);

	// Figure out where the application base path is
	char* argc_real_cstr = realpath(progname, nullptr);
	if (argc_real_cstr) {
		base_path = dirname(argc_real_cstr);
		free(argc_real_cstr);
	} else {
		logerror("Failed to determine realpath of '%s'", progname);
	}

	Anchor::self = this;
}

Anchor::~Anchor() {
	close(fifo_fd);
	fclose(log_fd);

	if (pid > 0)
		unlink(fifo_path.c_str());

	Anchor::self = nullptr;
}

void Anchor::log(Anchor::LogLevel lvl, const char* format, ...) const {
#ifdef NDEBUG
	if (lvl == Debug)
		return;
#endif

	va_list vargs;
	va_start(vargs, format);

	flockfile(log_fd);

	switch (lvl) {
		case Info:
			fputs("Info: ", log_fd);
			break;

		case Warn:
			fputs("\033[33mWarning\033[0m: ", log_fd);
			break;

		case Error:
			fputs("\033[31mError\033[0m: ", log_fd);
			break;

		default:
			fputs("\033[35mDebug\033[0m: ", log_fd);
			break;
	}

	vfprintf(log_fd, format, vargs);
	fputc('\n', log_fd);
	fflush(log_fd);

	funlockfile(log_fd);

	va_end(vargs);
}

}
