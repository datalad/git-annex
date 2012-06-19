int init_kqueue();
void addfds_kqueue(const int kq, const int fdcnt, const int *fdlist);
signed int waitchange_kqueue(const int kq);
