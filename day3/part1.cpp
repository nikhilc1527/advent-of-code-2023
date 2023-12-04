// solution: 530495

#include <bits/stdc++.h>

int main(int argc, char *argv[]) {
  std::vector<std::vector<char>> chars;

  const std::string file_name = "input";

  std::ifstream infile(file_name);
  {
    std::string line;
    while (std::getline(infile, line)) {
      chars.emplace_back();
      for (auto c : line) {
        chars[chars.size() - 1].push_back(c);
      }
    }
  }

  int n = chars.size(), m = chars[0].size();
  int i = 0, j = 0;
  int res = 0;
  auto is_numeric = [](char c) {return c >= '0' && c <= '9';};
  auto in_bounds = [n, m](int x, int y) {return x >= 0 && y >= 0 && x < n && y < m;};
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      if (is_numeric(chars[i][j])) {
        int y = j;
        while (in_bounds(i, y) && is_numeric(chars[i][y])) y++;
        bool use_num = false;
        for (int u = i - 1; u <= i + 1; u++) {
          for (int v = j - 1; v <= y; v++) {
            if (in_bounds(u, v) && chars[u][v] != '.' && !is_numeric(chars[u][v])) {
              use_num = true;
              // std::cout << i << ' ' << j << "\n";
              break;
            }
          }
        }
        if (use_num) {
          int num = 0;
          for (int v = j; v < y; v++) {
            num = num * 10 + (chars[i][v] - '0');
          }
          // std::cout << num << "\n";
          res += num;
          j = y;
        }
      }
    }
  }

  std::cout << res << "\n";

  return 0;
}
