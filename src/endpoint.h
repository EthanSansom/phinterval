#ifndef PHINTERVAL_ENDPOINT_H_
#define PHINTERVAL_ENDPOINT_H_

#include <vector>

struct Endpoint {
  bool is_start {};
  double value {};
};

using Endpoints = std::vector<Endpoint>;

bool operator< (const Endpoint &a, const Endpoint &b);

#endif
