#ifndef PHINTERVAL_ENDPOINT_H_
#define PHINTERVAL_ENDPOINT_H_

#include <vector>

struct Endpoint {
  bool is_start {};
  double value {};
};
struct BinaryEndpoint {
  bool is_start {};
  bool in_x {};
  double value {};
};

using Endpoints = std::vector<Endpoint>;
using BinaryEndpoints = std::vector<BinaryEndpoint>;

bool operator< (const Endpoint &a, const Endpoint &b);
bool operator< (const BinaryEndpoint &a, const BinaryEndpoint &b);

#endif
