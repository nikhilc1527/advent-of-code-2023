#include <bits/stdc++.h>

enum DIR {LEFT, RIGHT, UP, DOWN};

std::tuple<int, int, DIR> left_side = {0, -1, LEFT};
std::tuple<int, int, DIR> right_side = {0, 1, RIGHT};
std::tuple<int, int, DIR> up_side = {-1, 0, UP};
std::tuple<int, int, DIR> down_side = {1, 0, DOWN};

int path_len(const auto &chars, int x, int y, DIR dir) {
  if (chars[x][y] == 'S') return 1;

  const auto c = chars[x][y];

  std::tuple<int, int, DIR> next_dir;

  if (c == '|' && dir == UP) {
    next_dir = up_side;
  }
  else if (c == '|' && dir == DOWN) {
    next_dir = down_side;
  }
  else if (c == '-' && dir == LEFT) {
    next_dir = left_side;
  }
  else if (c == '-' && dir == RIGHT) {
    next_dir = right_side;
  }
  else if (c == 'F' && dir == UP) {
    next_dir = right_side;
  }
  else if (c == 'L' && dir == DOWN) {
    next_dir = right_side;
  }
  else if (c == 'F' && dir == LEFT) {
    next_dir = down_side;
  }
  else if (c == '7' && dir == RIGHT) {
    next_dir = down_side;
  }
  else if (c == 'J' && dir == DOWN) {
    next_dir = left_side;
  }
  else if (c == '7' && dir == UP) {
    next_dir = left_side;
  }
  else if (c == 'J' && dir == RIGHT) {
    next_dir = up_side;
  }
  else if (c == 'L' && dir == LEFT) {
    next_dir = up_side;
  }
  else {
    std::cout << "non exhaustive directions" << "\n";
    exit(1);
  }

  auto [nx, ny, d] = next_dir;

  auto next = path_len(chars, x+nx, y+ny, d);

  return next + 1;
}

int main() {
  std::vector<std::vector<char>> chars;

  const std::string file_name = "input";

  size_t sx, sy;

  std::ifstream infile(file_name);
  {
    std::string line;
    while (std::getline(infile, line)) {
      chars.emplace_back();
      for (auto c : line) {
        chars[chars.size() - 1].push_back(c);
        if (c == 'S') {
          sx = chars.size() - 1;
          sy = chars[chars.size()-1].size() - 1;
        }
      }
    }
  }

  auto n = chars.size(), m = chars[0].size();
  auto in_bounds = [n, m](auto x, auto y) {return x >= 0 && y >= 0 && x < n && y < m;};

  std::tuple<int, int, DIR> dir;
  if (auto c = chars[sx-1][sy]; in_bounds(sx-1, sy) && (c == '|' || c == '7' || c == 'F')) {
    dir = up_side;
  }
  else if (auto c = chars[sx+1][sy]; in_bounds(sx+1, sy) && (c == '|' || c == 'J' || c == 'L')) {
    dir = down_side;
  }
  else if (auto c = chars[sx][sy-1]; in_bounds(sx, sy-1) && (c == '-' || c == 'L' || c == 'F')) {
    dir = left_side;
  }
  else if (auto c = chars[sx][sy+1]; in_bounds(sx, sy+1) && (c == '-' || c == 'J' || c == '7')) {
    dir = right_side;
  }

  auto [nx, ny, d] = dir;
  auto res = path_len(chars, sx+nx, sy+ny, d);

  std::cout << res/2 << "\n";
}
