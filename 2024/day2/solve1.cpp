#include <iostream>
#include <string>
#include <vector>
#include <iterator>
#include <algorithm>

#include <sstream>

using namespace std;

template <typename Iterator>
bool is_safe(Iterator fst, Iterator lst) {
    typename iterator_traits<Iterator>::value_type prev = *fst;
    int incdec;
    ++fst;

    if (prev < *fst && *fst-prev <= 3)
        incdec = 1;
    else if (prev > *fst && prev-*fst <= 3)
        incdec = -1;
    else
        return false;

    for (; fst != lst; ++fst) {
        typename iterator_traits<Iterator>::value_type diff = ((*fst - prev) * incdec);

        if (diff > 3 || diff < 1)
            return false;
        prev = *fst;
    }
    return true;
}


int main() {
    string line;
    int safe = 0;
    while (getline(cin, line)) {
        istringstream stream(line);
        vector<int> report;

        copy(istream_iterator<int>(stream), istream_iterator<int>(), back_inserter(report));
/*
        cout << "report:" << endl;
        copy(report.begin(), report.end(), ostream_iterator<int>(cout, " "));
*/
        safe += is_safe(report.begin(), report.end());// ? safe + 1 : safe;
    }

    cout << safe << endl;

    return 0;
}