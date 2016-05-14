#ifndef OLEC_CHAN_HPP_
#define OLEC_CHAN_HPP_

#include "common.hpp"
#include <utility>
#include <vector>
#include <list>
#include <thread>
#include <mutex>
#include <condition_variable>

OLEC_NS_BEGIN

struct WorkerPool {
	std::vector<std::thread> workers;

	template <typename F, typename... A>
	WorkerPool(size_t threads, F&& func, A&&... args):
		workers(threads)
	{
		for (size_t i = 0; i < threads; i++)
			workers[i] = std::thread(std::forward<F>(func), std::forward<A>(args)...);
	}

	~WorkerPool() {
		for (auto& worker: workers)
			if (worker.joinable())
				worker.join();
	}
};

struct Worker {
	std::thread worker;

	template <typename F, typename... A>
	Worker(F&& func, A&&... args):
		worker(std::forward<F>(func), std::forward<A>(args)...)
	{}

	~Worker() {
		if (worker.joinable())
			worker.join();
	}
};

template <typename T>
struct Channel {
	std::mutex m;
	std::condition_variable cv;
	std::list<T> items;

	void write(const T& item) {
		{
			std::unique_lock<std::mutex> lock(m);
			items.push_back(item);
		}

		cv.notify_one();
	}

	void read(T& output) {
		{
			std::unique_lock<std::mutex> lock(m);
			cv.wait(lock, [&]() {
				return !items.empty();
			});

			output = items.front();
			items.pop_front();
		}

		cv.notify_one();
	}

	T read() {
		std::unique_lock<std::mutex> lock(m);
		cv.wait(lock, [&]() {
			return !items.empty();
		});

		T ret = items.front();
		items.pop_front();

		lock.unlock();
		cv.notify_one();

		return ret;
	}
};

OLEC_NS_END

#endif
