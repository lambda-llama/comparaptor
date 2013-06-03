int safe_memcmp(unsigned char* l, unsigned char* r, int size) {
    int c;
    int result = 0;

    for(c=0;c<size;c++){
        int diff = * l - * r;
        int diff_pos = diff > 0;
        int diff_neg = - ( diff < 0 );

        unsigned mask = - ( result == 0u );
        result |= ( diff_pos | diff_neg ) & mask;

        r++;
        l++;
    }
    return result;
}
