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

template <typename Iterator>
bool is_safe2(Iterator fst, Iterator lst) {
    if (is_safe(fst, lst))
        return true;

    for (Iterator to_del = fst; to_del != lst; ++to_del) {        
        vector<typename iterator_traits<Iterator>::value_type> tmp;
        copy(fst, to_del, back_inserter(tmp));
        copy(to_del+1, lst, back_inserter(tmp));
        if (is_safe(tmp.begin(), tmp.end()))
            return true;
    }
    return false;
}

int main() {
    string line;
    int safe = 0;
    while (getline(cin, line)) {
        istringstream stream(line);
        vector<int> report;

        copy(istream_iterator<int>(stream), istream_iterator<int>(), back_inserter(report));

        cout << "report: ";
        copy(report.begin(), report.end(), ostream_iterator<int>(cout, " "));
        
        bool s = is_safe2(report.begin(), report.end());
        cout << " is safe? " << s << endl;

        safe += s;
    }

    cout << safe << endl;

    return 0;
}