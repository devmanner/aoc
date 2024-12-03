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
    int incdec = 0;
    int errors = 0;


    for (++fst; fst != lst; ++fst) {
        if (*fst == prev && incdec == 0)
            errors++;
        else {
            if (incdec == 0) {
                incdec = 1;
                if (*fst < prev)
                    incdec = -1;
            }
            typename iterator_traits<Iterator>::value_type diff = ((*fst - prev) * incdec);
            if (diff > 3 || diff < 1)
                errors++;
        }
        prev = *fst;
    }
    
    return errors <= 1;
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
        cout << " is safe? " << is_safe(report.begin(), report.end()) << endl;

        safe += is_safe(report.begin(), report.end());// ? safe + 1 : safe;
    }

    cout << safe << endl;

    return 0;
}