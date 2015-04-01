#include "anchor.h"

#include <cassert>
#include <cstdarg>

#include <pty.h>
#include <fcntl.h>
#include <sys/stat.h>

using namespace std;

namespace olec {

static inline
bool send_entirely(int fd, const void* data, size_t rem) {
	const uint8_t* pos = (const uint8_t*) data;
	const uint8_t* end = pos + rem;

	while (pos != end) {
		ssize_t r = ::write(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}

static inline
bool receive_entirely(int fd, void* data, size_t rem) {
	uint8_t* pos = (uint8_t*) data;
	uint8_t* end = pos + rem;

	while (pos != end) {
		ssize_t r = ::read(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}

Anchor::Anchor():
	fifo_path("/tmp/olec-" + to_string(getpid()))
{
	assert(mkfifo(fifo_path.c_str(), S_IWUSR | S_IRUSR) == 0);

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

	char* env_home = getenv("HOME");
	string log_path;

	if (env_home) {
		log_path = string(env_home) + "/.olec.log";
	} else {
		log_path = "olec.log";
	}

	log_fd = fopen(log_path.c_str(), "a+");
	assert(log_fd != nullptr);
}

Anchor::~Anchor() {
	close(fifo_fd);
	fclose(log_fd);
	if (pid > 0) unlink(fifo_path.c_str());
}

bool Anchor::send(const Event& ev) const {
	return send_entirely(fifo_fd, &ev, sizeof(Event));
}

bool Anchor::receive(Event& ev) const {
	return receive_entirely(fifo_fd, &ev, sizeof(Event));
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
