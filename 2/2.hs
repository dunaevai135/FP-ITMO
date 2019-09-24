long my_hash(long long value, long ht_size) {
return abs((value * 47) ^ (value * 31)) % ht_size;
}

