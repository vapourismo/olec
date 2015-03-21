#include "ipc.h"

#include <sstream>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

using namespace std;

namespace olec {

// bool CommChannel::create(const std::string& path) {
// 	return mkfifo(path.c_str(), S_IFIFO | S_IWUSR | S_IRUSR) == 0;
// }

bool ipc_send(int fd, const Event& event) {
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

bool ipc_receive(int fd, Event& event) {
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
