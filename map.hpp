#ifndef _map_hpp_
#define _map_hpp_

#include <inttypes.h>

#include <cstddef>
#include <functional>
#include <algorithm>
#include <vector>
#include <utility>
#include <iostream>
#include <cassert>

#define H 32


enum bucket_status {
	Empty = 0,
	Occupied,
	ReHashed
};

template<
	typename Key,
	typename Val
	>
struct bucket {
	Key                     key;
	Val                     val;
	enum bucket_status      status;
	uint32_t                hop;
	// TODO: add copy and move constructors so that swapping is move
	// intuitive and faster.

	// Copy the contents of another bucket over while retaining the hop
	// bitmap.
	bucket &operator<<=(const bucket &rhs) {
		key = rhs.key;
		val = rhs.val;
		status = rhs.status;
	}
};

template<
	typename Key,
	typename Val,
	typename Hash,
	typename KeyEqual,
	typename Allocator
	>
struct map;


template<
	typename Key,
	typename Val,
	typename Hash,
	typename KeyEqual,
	typename Allocator
	>
struct map_iterator {
	bucket<Key, Val>                                *curr;
	map<Key, Val, Hash, KeyEqual, Allocator>        *mp;

	map_iterator(bucket<Key, Val> *nb,
		     map<Key, Val, Hash, KeyEqual, Allocator> *nm)
		: curr(nb), mp(nm) {}

	map_iterator() {}

	Val &operator*() const {
		return curr->val;
	}

	Val *operator->() const {
		return &(operator*());
	}

	map_iterator &operator++() {
		if (curr != nullptr && curr != mp->buckets.end()) {
			do {
				curr++;
			} while (curr != mp->buckets.end() && !curr->occupied);
		}
		return *this;
	}

	map_iterator operator++(int) {
		auto tmp = *this;
		++*this;
		return tmp;
	}

	bool operator==(const map_iterator &it) const {
		return curr == it.curr;
	}

	bool operator!=(const map_iterator &it) const {
		return curr != it.curr;
	}
};

template<
	typename Key,
	typename Val,
	typename Hash = std::hash<Key>,
	typename KeyEqual = std::equal_to<Key>,
	typename Allocator = std::allocator<bucket<Key, Val>>
	>
struct map {
	typedef Key                             key_type;
	typedef Val                             mapped_type;
	typedef std::pair<const Key, Val>       value_type;
	typedef map_iterator<Key, Val, Hash, KeyEqual, Allocator>
	                                        iterator;
	typedef std::size_t                     size_t;

	const Hash                                      hash_fn;
	const KeyEqual                                  is_equal;
	std::vector<bucket<Key, Val>, Allocator>        buckets;
	size_t                                          num_elements;

	map(size_t n = 79,
	    const Hash& hf = Hash(),
	    const KeyEqual& eql = KeyEqual(),
	    const Allocator& a = Allocator())
		: hash_fn(hf), is_equal(eql), buckets(n, a), num_elements(0)
		{}

	map(const map&) = default;

	map(map&&) = default;

	map& operator=(const map&) = default;

	map& operator=(map&&) = default;


	bool empty() {
		return num_elements == 0;

	}

	size_t size() {
		return num_elements;
	}

	iterator
	begin() {
		for (size_t i = 0; i < buckets.size(); i++) {
			if (buckets[i].occupied) {
				return iterator(&buckets[i], this);
			}
		}
		return end();
	}

	iterator
	end() {
		return iterator(nullptr, this);
	}

	void
	erase(const key_type& key) {
		auto bp = lookup(key);
		if (bp != nullptr) {
			bp->status = Empty;
			num_elements--;
		}
	}

	iterator
	find(const key_type& key) {
		auto bp = lookup(key);
		return iterator(bp, this);
	}


	std::pair<iterator, bool>
	insert(const value_type& val) {
		if (buckets.size() == 0) {
			// This isn't a good first size, but whatever.
			buckets.resize(H << 1);
		}
	insert_after_resize:
		auto hash = hash_fn(val.first) % buckets.size();
		auto *dst = &buckets[hash];
		auto curr_hop = dst->hop;
		size_t base, dist;
		// Linearly probe for an unoccupied spot and a matching key.
		if (hash >= buckets.size() - (H-1)) {
			// We will encounter overflow. Handle it.
			size_t i, j;
			bucket<Key, Val> *open_spot = nullptr;
			auto lim = buckets.size() - hash;
			for (i = 0; i < lim; i++) {
				auto *curr = &buckets[hash + i];
				if (curr->status == Empty) {
					if (open_spot == nullptr) {
						open_spot = curr;
						dist = i;
					}
				} else if ((curr_hop & (1 << i)) &&
					   is_equal(val.first, curr->key)) {
					return std::make_pair(
						iterator(curr, this), false);
				}
			}
			// Wrap back to the lower bucket entries.
			for (j = 0; j < H - lim; j++) {
				auto *curr = &buckets[j];
				if (curr->status == Empty) {
					if (open_spot == nullptr) {
						open_spot = curr;
						dist = i + j;
					}
				} else if ((curr_hop & (1 << (i + j))) &&
					   is_equal(val.first, curr->key)) {
					return std::make_pair(
						iterator(curr, this), false);
				}
			}
			// If we have an open spot, set it. Otherwise we must
			// swap.
			if (open_spot != nullptr) {
				open_spot->key = val.first;
				open_spot->val = val.second;
				open_spot->status = Occupied;
				dst->hop |= (1 << dist) & 0xffffffff;
				assert(dst->hop <= 0xffffffff);
				num_elements++;
				return std::make_pair(iterator(open_spot, this),
						 true);
			}
			// Find the first unoccupied spot outside the hop
			// distance. We know that base < hash.
			for (base = j; base < hash; base++) {
				if (buckets[base].status == Empty) {
					dist = base + buckets.size() - hash;
					goto swap_step;
				}
			}
			// There are no available buckets. We must resize.
			resize();
			goto insert_after_resize;
		} else {
			// No overflow will be encountered.
			size_t i;
			bucket<Key, Val> *open_spot = nullptr;
			for (i = 0; i < H; i++) {
				auto *curr = &buckets[hash + i];
				if (curr->status == Empty) {
					if (open_spot == nullptr) {
						open_spot = curr;
						dist = i;
					}
				} else if ((curr_hop & (1 << i)) &
					   is_equal(val.first, curr->key)) {
					return std::make_pair(
						iterator(curr, this), false);
				}
			}
			if (open_spot != nullptr) {
				open_spot->key = val.first;
				open_spot->val = val.second;
				open_spot->status = Occupied;
				dst->hop |= (1 << dist) & 0xffffffff;
				assert(dst->hop <= 0xffffffff);
				num_elements++;
				return std::make_pair(iterator(open_spot, this), true);
			}
			// Find the first unoccupied spot outside the hop
			// distance. In this case we know that base > hash.
			for (base = i; base < buckets.size(); base++) {
				if (buckets[base].status == Empty) {
					dist = base - hash;
					goto swap_step;
				}
			}
			for (base = 0; base < hash; base++) {
				if (buckets[base].status == Empty) {
					dist = base + buckets.size() - hash;
					goto swap_step;
				}
			}
			// There are no availabe buckets.
			resize();
			goto insert_after_resize;
		}
	swap_step:
		// Our goal here is to swap the base entry with entries
		// closer to our hop distance until we are within our hop
		// distance.
		assert(base < buckets.size());
		if ((H - 1) <= base) {
			// No underflow will be immediately encountered.
		no_underflow:
			auto lower_lim = base - (H-1);
			for (;;) {
				size_t i, j;
				auto upper_lim = H-1;
				for (i = lower_lim; i < base; i++) {
					for (j = 0; j < upper_lim; j++) {
						if (buckets[i].hop & (1 << j))
							goto found_swap;
					}
					upper_lim--;
				}
				// No suitable bucket found to swap.
				resize();
				goto insert_after_resize;
			found_swap:
				assert(i+j < buckets.size());
				assert(base < buckets.size());
				buckets[base] <<= buckets[i + j];
				buckets[i + j].status = Empty;
				buckets[i].hop &= ~(1 << j);
				buckets[i].hop |= (1 << base - i) & 0xffffffff;
				assert(buckets[i].hop <= 0xffffffff);
				assert(dst->hop <= 0xffffffff);

				size_t new_base = i + j;
				size_t delta = base - new_base;
				if (dist <= delta || dist - delta < H) {
					auto curr = &buckets[new_base];
					curr->key = val.first;
					curr->val = val.second;
					curr->status = Occupied;
					dst->hop |= (1 << (dist - delta)) & 0xffffffff;
					assert(dst->hop <= 0xffffffff);
					num_elements++;
					return std::make_pair(
						iterator(curr, this),
						true);
				}
				dist -= delta;
				base = new_base;
				if (lower_lim < delta) {
					// We will encounter underflow.
					break;
				}
				lower_lim -= delta;
			}
		}
		// Underflow found.
		auto lower_lim = buckets.size() - ((H - 1) - base);
		for (;;) {
			size_t i, j, delta, new_base;
			auto upper_lim = H - 1;
			for (i = lower_lim; i < buckets.size(); i++) {
				for (j = 0; j < buckets.size() - i; j++) {
					if (buckets[i].hop & (1 << j)) {
						buckets[base] <<= buckets[i+j];
						assert(i+j < buckets.size());
						buckets[i + j].status = Empty;
						assert(buckets[i].hop <= 0xffffffff);
						buckets[i].hop &= ~(1 << j);
						buckets[i].hop |= ((1 << base +
								    buckets.size() - i)
								   & 0xffffffff);
						delta = buckets.size() - i + base - j;
						assert(buckets[i].hop <= 0xffffffff);
						new_base = i + j;
						goto found_swap_underflow;
					}
				}
				for (size_t k = 0; j + k < upper_lim; k++) {
					if (buckets[i].hop & (1 << j + k)) {
						buckets[base] <<= buckets[k];
						assert(k + j < H-1);
						buckets[k].status = Empty;
						assert(buckets[i].hop <= 0xffffffff);
						buckets[i].hop &= ~(1 << j + k);
						buckets[i].hop |= ((1 << base +
								    buckets.size() - i)
								   & 0xffffffff);
						delta = base - k;
						assert(buckets[i].hop <= 0xffffffff);
						new_base = k;
						goto found_swap_underflow;
					}
				}
				upper_lim--;
			}
			for (i = 0; i < base; i++) {
				for (j = 0; j < upper_lim; j++) {
					if (buckets[i].hop & (1 << j)) {
						// no underflow
						assert(i+j < buckets.size());
						buckets[base] <<= buckets[i + j];
						buckets[i + j].status = Empty;
						buckets[i].hop &= ~(1 << j);
						buckets[i].hop |= (1 << base - i) & 0xffffffff;
						delta = base - (i + j);
						assert(buckets[i].hop <= 0xffffffff);
						new_base = i + j;
						goto found_swap_underflow;
					}
				}
				upper_lim--;
			}
			// No suitable bucket found to swap.
			resize();
			goto insert_after_resize;
		found_swap_underflow:
			if (dist <= delta || dist - delta < H) {
				// Found a suitable bucket.
				auto curr = &buckets[new_base];
				curr->key = val.first;
				curr->val = val.second;
				curr->status = Occupied;
				dst->hop |= (1 << (dist - delta)) & 0xffffffff;
				num_elements++;
				return std::make_pair(
					iterator(curr, this),
					true);
			}
			dist -= delta;
			lower_lim -= delta;
			base = new_base;
			if ((H - 1) <= base) {
				goto no_underflow;
			}
		}
	}

	size_t
	find_linear(Val val) {
		for (size_t i = 0; i < buckets.size(); i++) {
			if (buckets[i].val == val && buckets[i].status != Empty) {
				return i;
			}
		}
		return 0;
	}

private:
	bucket<Key, Val> *
	lookup(const key_type& key) {
		auto hash = hash_fn(key) % buckets.size();
		auto *dst = &buckets[hash];
		auto curr_hop = dst->hop;
		if (hash >= buckets.size() - (H-1)) {
			// We will encounter overflow. Handle it.
			size_t i;
			auto lim = buckets.size() - hash;
			for (i = 0; i < lim; i++) {
				auto *curr = &buckets[hash + i];
			        if ((curr_hop & (1 << i)) &&
					   is_equal(key, curr->key)) {
					return curr;
				}
			}
			// Wrap back to the lower bucket entries.
			for (size_t j = 0; j < H - lim; j++) {
				auto *curr = &buckets[j];
			        if ((curr_hop & (1 << (i + j))) &&
					   is_equal(key, curr->key)) {
					return curr;
				}
			}
		} else {
			// No overflow will be encountered.
			for (size_t i = 0; i < H; i++) {
				auto *curr = &buckets[hash + i];
			        if ((curr_hop & (1 << i)) &&
				   is_equal(key, curr->key)) {
					return curr;
				}
			}
		}
		return nullptr;
	}

	enum replacement_status {
		Failed_to_replace = 0,
		Replaced_no_chain,
		Replaced_chain,
	};

	void
	resize() {
		auto prev_size = buckets.size();
		// doubling the vector size isn't an amazing idea but I don't
		// want to write out a prime table right now.
		auto new_size = (prev_size << 1) + 1;
		buckets.resize(new_size);
		// Using a work queue like this is due to poor implementation;
		// I'm collecting all of the items I'm unable to reinsert
		// quickly and putting them through the slow user-facing insert
		// function later. That's bad, I'll fix it at some point.
		std::vector<std::pair<Key, Val>> queue;
		for (size_t i = 0; i < prev_size; i++) {
			switch (buckets[i].status) {
			case Occupied: {
				auto entry = std::make_pair(buckets[i].key,
						       buckets[i].val);
				auto hash = hash_fn(buckets[i].key);
				auto prev_place = hash % prev_size;
				auto next_place = hash % new_size;
				buckets[prev_place].hop &= ~(1 << i-prev_place);
				if (next_place == i) {
					buckets[next_place].hop |= 1;
					continue;
				}
				if (next_place < prev_size && next_place > i) {
					buckets[i].status = ReHashed;
				}
				switch (replace(prev_size, new_size,
						i, queue,
						next_place, i, 0)) {
				case Failed_to_replace:
					queue.push_back(entry);
				case Replaced_no_chain:
					buckets[i].status = Empty;
					break;
				case Replaced_chain:
					break;
				}
				break;
			}

			case ReHashed:
				buckets[i].status = Occupied;
				break;
			}
		}
		// Optimization: insert checks if entry already exists, this is
		// slow.
		for (auto entry : queue) {
			insert(entry);
		}
	}

	enum replacement_status
	replace(const size_t prev_size, const size_t new_size,
		const size_t initial, std::vector<std::pair<Key, Val>> &queue,
		size_t place, size_t replacement, size_t depth) {
		if (place == initial) {
			// This chain can be collapsed.
			buckets[initial] <<= buckets[replacement];
			buckets[initial].hop |= 1;
			return Replaced_chain;
		}
		switch (buckets[place].status) {
		case Empty:
			buckets[place] <<= buckets[replacement];
			buckets[place].hop |= 1;
			return Replaced_no_chain;
		case ReHashed:
			return Failed_to_replace;
		}
		// The bucket is occupied.
		if (place < initial || place >= prev_size) {
			// places before our current marker or
			// past prev_size have already been
			// moved. We should avoid touching them.
			return Failed_to_replace;
		}
		// Naively attempt to move next to an unaccopied
		// space.
		auto entry = std::make_pair(buckets[place].key,
					    buckets[place].val);
		auto hash = hash_fn(buckets[place].key);
		auto prev_place = hash % prev_size;
		auto next_place = hash % new_size;
		if (next_place < prev_size && next_place > initial) {
			// Mark this entry as rehashed so that no future
			// replacements will displace it.
			buckets[place].status = ReHashed;
		}
		if (next_place == place) {
			return Failed_to_replace;
		}
		// We should add a heuristic check if next_place
		// is within the hop distance of i and add
		// it there, completing  a chain.
		auto repl = buckets[replacement];
		repl.hop = buckets[place].hop;
		auto status = (depth < 1000)
			? replace(prev_size, new_size,
				  initial, queue,
				  next_place, place, depth + 1)
			: Failed_to_replace;
		if (status == Failed_to_replace) {
			// If we could find a spot, add this
			// entry to the queue so we can add
			// it later.
			queue.push_back(entry);
			status = Replaced_no_chain;
		}
		buckets[place] = repl;
		buckets[prev_place].hop &= ~(1 << place-prev_place);
		buckets[place].hop |= 1;
		return status;
	}
};

#endif
