#include <list>
#include <algorithm>
#include <iostream>

using namespace std;

typedef list<pair <int, int> > list_t;
typedef list_t::iterator itr_t;

void next(list_t &circle, itr_t &itr) {
    ++itr;
    if (itr == circle.end())
        itr = circle.begin();
}

void play_round(list_t &circle, itr_t player, itr_t opponent) {
    while (player != circle.end() && circle.size() != 1) {
        player->second += opponent->second;
        ++player;
        itr_t to_delete = opponent;
        next(circle, opponent);
        if (circle.size() & 1)
            next(circle, opponent);
        circle.erase(to_delete);
    }
}

struct init {
    int idx;
    init() : idx(1) {}
    void operator()(pair<int, int> &p) { p = pair<int, int>(idx++, 1); }
};

int play(int circle_size) {
    // pair<ID, NPresents>
    list_t circle(circle_size);
    for_each(circle.begin(), circle.end(), init());

    while (circle.size() != 1) {
        itr_t itr = circle.begin();
        itr_t opp = circle.begin();
        advance(opp, circle.size() / 2);

        cout << "Number of players playing a round: " << circle.size() << endl;
        play_round(circle, itr, opp);
    }

    return circle.front().first;
}

void test() {
    cout << "Test starting...." << endl;

    assert(2 == play(5));

    cout << "Test end" << endl;
}


int main() {
    test();

    int winner = play(3012210);
    cout << "Winner ID: " << winner << ((winner == 1417887) ? " correct!" : " wrong...") << endl;

    return 0;
}
