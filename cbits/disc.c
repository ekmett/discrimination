#include "disc.h"

// haskell mutvar slot access
void c_snapshotArray(const void ** input, intptr_t * output, int len) {
  for (int i = 0; i < len; ++i) {
    output[i] = (intptr_t)(input[i]);
  }
}

// strefs

void c_snapshotList(const void ** input, intptr_t * output, int len) {

}

