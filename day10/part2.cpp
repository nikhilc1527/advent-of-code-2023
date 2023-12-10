#include <bits/stdc++.h>

enum DIR {LEFT, RIGHT, UP, DOWN};

std::tuple<int, int, DIR> left_side = {0, -1, LEFT};
std::tuple<int, int, DIR> right_side = {0, 1, RIGHT};
std::tuple<int, int, DIR> up_side = {-1, 0, UP};
std::tuple<int, int, DIR> down_side = {1, 0, DOWN};

using chars_type = std::vector<std::vector<char>>;
using new_chars_type = std::vector<std::vector<char>>;

char path_char = '|';
char inner_char = '0';

void create_path(const chars_type &chars, int x, int y, DIR dir, new_chars_type &new_chars) {
  new_chars[x][y] = path_char;

  if (chars[x][y] == 'S') return;

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

  create_path(chars, x+nx, y+ny, d, new_chars);
}


chars_type expand(const chars_type &chars, size_t n, size_t m) {
  chars_type new_chars(n * 3, std::vector(m * 3, inner_char));

  for (auto i = 0; i < n; ++i) {
    for (auto j = 0; j < m; ++j) {
      auto x=i*3, y=j*3;
      switch (chars[i][j]) {
      case 'L':
        new_chars[x][y+1] = '|';
        new_chars[x+1][y+1] = 'L';
        new_chars[x+1][y+2] = '-';
        break;
      case 'J':
        new_chars[x][y+1] = '|';
        new_chars[x+1][y+1] = 'J';
        new_chars[x+1][y] = '-';
        break;
      case '7':
        new_chars[x+1][y] = '-';
        new_chars[x+1][y+1] = '7';
        new_chars[x+2][y+1] = '|';
        break;
      case 'F':
        new_chars[x+1][y+2] = '-';
        new_chars[x+1][y+1] = 'F';
        new_chars[x+2][y+1] = '|';
        break;
      case '|':
        new_chars[x][y+1] = '|';
        new_chars[x+1][y+1] = '|';
        new_chars[x+2][y+1] = '|';
        break;
      case '-':
        new_chars[x+1][y+0] = '-';
        new_chars[x+1][y+1] = '-';
        new_chars[x+1][y+2] = '-';
        break;
      }
    }
  }
  
  return new_chars;
}

void flood_fill(chars_type &chars, int i, int j) {

  auto n = chars.size();
  auto m = chars[0].size();
  auto in_bounds = [&n, &m](auto x, auto y) {return x >= 0 && y >= 0 && x < n && y < m;};

  if (!in_bounds(i, j) || chars[i][j] != inner_char) return;

  chars[i][j] = ' ';
  flood_fill(chars, i+1, j);
  flood_fill(chars, i, j+1);
  flood_fill(chars, i-1, j);
  flood_fill(chars, i, j-1);
}

int main() {
  chars_type old_chars;

  const std::string file_name = "example4";

  size_t sx, sy;

  std::ifstream infile(file_name);
  {
    std::string line;
    while (std::getline(infile, line)) {
      old_chars.emplace_back();
      for (auto c : line) {
        old_chars[old_chars.size() - 1].push_back(c);
        if (c == 'S') {
          sx = old_chars.size() - 1;
          sy = old_chars[old_chars.size()-1].size() - 1;
        }
      }
    }
  }

  auto n = old_chars.size(), m = old_chars[0].size();
  auto print_arr = [](const auto &arr) {
    for (auto i = 0; i < arr.size(); ++i) {
      for (auto j = 0; j < arr[i].size(); ++j) {
        std::cout << arr[i][j];
      }
      std::cout << '\n';
    }
  };
  auto in_bounds = [&n, &m](auto x, auto y) {return x >= 0 && y >= 0 && x < n && y < m;};
  // print_arr(old_chars);

  auto chars = expand(old_chars, old_chars.size(), old_chars[0].size());
  n = chars.size(); m = chars[0].size();
  sx = sx*3+1;
  sy = sy*3+1;
  char s_repl;
  if (in_bounds(sx-2, sy-2) && chars[sx-2][sy] == '|' && chars[sx][sy-2] == '-') {
    s_repl = 'J';
    chars[sx-1][sy] = '|';
    chars[sx][sy-1] = '-';
  }
  else if (in_bounds(sx+2, sy-2) && chars[sx+2][sy] == '|' && chars[sx][sy-2] == '-') {
    s_repl = '7';
    chars[sx+1][sy] = '|';
    chars[sx][sy-1] = '-';
  }
  if (in_bounds(sx-2, sy+2) && chars[sx-2][sy] == '|' && chars[sx][sy+2] == '-') {
    s_repl = 'L';
    chars[sx-1][sy] = '|';
    chars[sx][sy+1] = '-';
  }
  else if (in_bounds(sx+2, sy+2) && chars[sx+2][sy] == '|' && chars[sx][sy+2] == '-') {
    s_repl = 'F';
    chars[sx+1][sy] = '|';
    chars[sx][sy+1] = '-';
  }

  chars[sx][sy] = 'S';

  // print_arr(chars);
  
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

  new_chars_type new_chars = std::vector(n, std::vector(m, ' '));
  
  auto [nx, ny, d] = dir;
  create_path(chars, sx+nx, sy+ny, d, new_chars);
  // print_arr(new_chars);
  new_chars_type filled = new_chars;
  
  for (auto i = 0; i < n; i++) {
    for (auto j = 0; j < m; j++) {
      if (filled[i][j] != path_char) {
        filled[i][j] = inner_char;
      }
    }
  }
  flood_fill(filled, 0, 0);
  // print_arr(filled);

  int res = 0;
  for (int i = 1; i < n; i += 3) {
    for (int j = 1; j < m; j += 3) {
      if (filled[i][j] == inner_char) res++;
    }
  }
  std::cout << res << "\n";
}
