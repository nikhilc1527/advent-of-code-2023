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
  std::uint64_t res = 0;
  auto is_numeric = [](char c) {return c >= '0' && c <= '9';};
  auto in_bounds = [n, m](int x, int y) {return x >= 0 && y >= 0 && x < n && y < m;};
  auto get_number = [&chars, in_bounds, is_numeric](int i, int j) {
    int v = j-1;
    while (in_bounds(i, v) && is_numeric(chars[i][v])) v--;
    int num = 0;
    for (int y = v+1; in_bounds(i, y) && is_numeric(chars[i][y]); y++) {
      num = num * 10 + chars[i][y] - '0';
    }
    return num;
  };
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      std::vector<int> nums;
      if (chars[i][j] == '*') {
        if (in_bounds(i-1, j) && is_numeric(chars[i-1][j])) {
          nums.push_back(get_number(i-1, j));
        }
        else {
          if (in_bounds(i-1, j-1) && is_numeric(chars[i-1][j-1])) {
            nums.push_back(get_number(i-1, j-1));
          }
          if (in_bounds(i-1, j+1) && is_numeric(chars[i-1][j+1])) {
            nums.push_back(get_number(i-1, j+1));
          }
        }
        if (in_bounds(i+1, j) && is_numeric(chars[i+1][j])) {
          nums.push_back(get_number(i+1, j));
        }
        else {
          if (in_bounds(i+1, j-1) && is_numeric(chars[i+1][j-1])) {
            nums.push_back(get_number(i+1, j-1));
          }
          if (in_bounds(i+1, j+1) && is_numeric(chars[i+1][j+1])) {
            nums.push_back(get_number(i+1, j+1));
          }
        }
        if (in_bounds(i, j-1) && is_numeric(chars[i][j-1])) {
          nums.push_back(get_number(i, j-1));
        }
        if (in_bounds(i, j+1) && is_numeric(chars[i][j+1])) {
          nums.push_back(get_number(i, j+1));
        }

        if (nums.size() == 2) {
          res += nums[0] * nums[1];
          // std::cout << i << ',' << j << "\n";
          // std::cout << nums[0] << ',' << nums[1] << "\n";
        }
      }
    }
  }

  std::cout << res << "\n";

  return 0;
}
