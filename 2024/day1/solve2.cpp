#include <iostream>
#include <vector>
#include <map>
#include <utility>

#include <cstdlib>

int main() {
    int n1, n2;
    std::vector<int> v;
    std::map<int, int> m;

    while(std::cin >> n1 >> n2) {
        v.push_back(n1);
        m[n2]++;
    }

    int ss = 0;
    for (std::vector<int>::size_type i = 0; i < v.size(); ++i) {
        ss += v[i] * m[v[i]];
    }

    std::cout << ss << std::endl;

    return 0;
}