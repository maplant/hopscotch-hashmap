#include "map.hpp"

typedef map<int64_t, int64_t> hash_t;
typedef map<std::string, int64_t> str_hash_t;
#define SETUP hash_t hash; str_hash_t str_hash;
#define INSERT_INT_INTO_HASH(key, value) hash.insert(hash_t::value_type(key, value))
#define DELETE_INT_FROM_HASH(key) hash.erase(key);
#define INSERT_STR_INTO_HASH(key, value) str_hash.insert(str_hash_t::value_type(key, value))
#define DELETE_STR_FROM_HASH(key) str_hash.erase(key);

#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>
#include <unordered_map>

double get_time(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec + (tv.tv_usec / 1000000.0);
}

char * new_string_from_integer(int num)
{
    int ndigits = num == 0 ? 1 : (int)log10(num) + 1;
    char * str = (char *)malloc(ndigits + 1);
    sprintf(str, "%d", num);
    return str;
}

int main(int argc, char ** argv)
{
    int num_keys = atoi(argv[1]);
    int i, value = 0;
    std::vector<std::string> items;
    std::unordered_map<std::string, bool> checker;
    if(argc <= 2)
        return 1;

    SETUP

    double before = get_time();

    if(!strcmp(argv[2], "sequential"))
    {
        for(i = 0; i < num_keys; i++)
            INSERT_INT_INTO_HASH(i, value);
    }

    else if(!strcmp(argv[2], "random"))
    {
        srandom(1); // for a fair/deterministic comparison
        for(i = 0; i < num_keys; i++)
            INSERT_INT_INTO_HASH((int)random(), value);
    }

    else if(!strcmp(argv[2], "delete"))
    {
        for(i = 0; i < num_keys; i++)
            INSERT_INT_INTO_HASH(i, value);
        before = get_time();
        for(i = 0; i < num_keys; i++)
            DELETE_INT_FROM_HASH(i);
    }

    else if(!strcmp(argv[2], "sequentialstring"))
    {
        for(i = 0; i < num_keys; i++)
            INSERT_STR_INTO_HASH(new_string_from_integer(i), value);
    }

    else if(!strcmp(argv[2], "randomstring"))
    {
	    srandom(time(NULL)); // for a fair/deterministic comparison
        for(i = 0; i < num_keys; i++) {
		for (;;) {
			std::string s(new_string_from_integer((int)random()));
			if (checker.find(s) == checker.end()) {
				checker.insert(std::make_pair(s, true));
				INSERT_STR_INTO_HASH(s, i);
				items.push_back(s);
				break;
			}
		}
	}
	std::cout << items.size() <<  "checking...\n";
	auto found_errors = false;
	for (i = 0; i < num_keys; i++) {
		auto it = str_hash.find(items[i]);
		if (it == str_hash.end()) {
			std::cout << "could not find #" << i << "\n";
			std::cout << "found at "<< str_hash.find_linear(i) << "\n";
			found_errors = true;
		} else if (*it != i)
			std::cout << "value miss-match, " << i << " != " << *it << "\n";
	}
	sleep(10);
	if (found_errors) {
		for (i = 0; i < num_keys; i++) {
			std::cout << "\"" << items[i] << "\",\n";
		}
		return 0;
	}
    }

    else if(!strcmp(argv[2], "deletestring"))
    {
        for(i = 0; i < num_keys; i++)
            INSERT_STR_INTO_HASH(new_string_from_integer(i), value);
        before = get_time();
        for(i = 0; i < num_keys; i++)
            DELETE_STR_FROM_HASH(new_string_from_integer(i));
    }

    double after = get_time();
    printf("%f\n", after-before);
    fflush(stdout);
    sleep(1000000);
}
