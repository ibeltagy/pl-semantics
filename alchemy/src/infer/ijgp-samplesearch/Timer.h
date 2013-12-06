/*
 * Timer.h
 *
 *  Created on: Jun 21, 2010
 *      Author: Vibhav Gogate
 *      Email: vgogate@cs.washington.edu
 *      University of Washington, Seattle
 *      All rights reserved.
 */

#ifndef SS_TIMER_H_
#define SS_TIMER_H_
#include <ctime>
using namespace std;

namespace ss{

struct Timer {
	Timer() {
	}
	clock_t start() {
		start_time = clock();
	}
	inline clock_t elapsed_seconds() {
		return (clock() - start_time) / CLOCKS_PER_SEC;
	}
	bool timeout(clock_t& seconds) {
		return seconds <= elapsed_seconds();
	}
private:
	clock_t start_time;
};
}
#endif /* TIMER_H_ */
