#include <iostream>
#include <vector>
#include <algorithm>

#include <cstdlib>

int main() {
    int n1, n2;
    std::vector<int> v1, v2;

    while(std::cin >> n1 >> n2) {
        v1.push_back(n1);
        v2.push_back(n2);
    }

    std::sort(v1.begin(), v1.end());
    std::sort(v2.begin(), v2.end());


    int dist = 0;
    for (std::vector<int>::size_type i = 0; i < v1.size(); ++i) {
        dist += abs(v1[i] - v2[i]);
    }

    std::cout << dist << std::endl;

    return 0;
}