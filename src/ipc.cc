#include "ipc.h"

#include <sstream>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

using namespace std;

namespace olec {

CommChannel::CommChannel(const std::string& path, bool make) {
	if (make && mkfifo(path.c_str(), S_IFIFO | S_IWUSR | S_IRUSR) != 0)
		throw;

	if ((fd = open(path.c_str(), O_WRONLY)) < 0)
		throw;

	if (make)
		clean_me = path;
}

CommChannel::~CommChannel() {
	if (fd >= 0) close(fd);

	if (!clean_me.empty())
		unlink(clean_me.c_str());
}

bool CommChannel::send(const Event& event) {
	const uint8_t* pos = (const uint8_t*) &event;
	const uint8_t* end = pos + sizeof(Event);
	size_t rem = sizeof(Event);

	while (pos != end) {
		ssize_t r = write(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}

bool CommChannel::receive(Event& event) {
	uint8_t* pos = (uint8_t*) &event;
	uint8_t* end = pos + sizeof(Event);
	size_t rem = sizeof(Event);

	while (pos != end) {
		ssize_t r = read(fd, pos, rem);

		if (r < 0)
			return false;

		pos += r;
		rem -= r;
	}

	return true;
}
}
