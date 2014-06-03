#include "tokenizer.h"
#include "aux.h"

tokpattern_t* tokpattern_new(size_t id, const char* preg) {
	tokpattern_t* tp = new(tokpattern_t);

	if (tp) {
		/* compile the regular expression */
		if (regcomp(&tp->pattern, preg, REG_EXTENDED) != 0) {
			free(tp);
			return NULL;
		}

		tp->id = id;
	}

	return tp;
}

void tokpattern_free(tokpattern_t* tp) {
	if (tp) {
		/* the regex needs to be freed seperately */
		regfree(&tp->pattern);

		free(tp);
	}
}

ssize_t tokpattern_check(const tokpattern_t* tp, const char* input) {
	regmatch_t m;

	if (regexec(&tp->pattern, input, 1, &m, 0) == 0) {
		/* since we do not want to skip input,
		   we have to make sure the match is at offset 0 */
		return m.rm_so == 0 ? m.rm_eo : -1;
	} else {
		return -1;
	}
}
