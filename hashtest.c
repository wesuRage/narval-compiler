#include <stdio.h>
#include <stdlib.h>

#define HASH_TABLE(T) \
  typedef struct { \
    unsigned int size; \
    int* keys; \
    T* values; \
    unsigned int capacity; \
  } HashTable_##T; \
  \
  HashTable_##T* ht_##T##_new(unsigned int cap) {\
    HashTable_##T* ht = (HashTable_##T*)malloc(sizeof(HashTable_##T)); \
    if (ht == NULL) return NULL; \
    ht->size = 0; \
    ht->keys = (int*)malloc(sizeof(int) * cap); \
    ht->values = (T*)malloc(sizeof(T) * cap); \
    ht->capacity = cap; \
    return ht; \
  } \
  \
  int ht_##T##_insert(HashTable_##T* self, int keylen, const char* key, T value) {\
    int hashed = hash(key, keylen) % self->capacity; \
    self->values[hashed] = value; \
    self->keys[hashed] = keylen; /* Simplicity assumption */ \
    self->size += 1; \
    return 0; \
  } \
  \
  T* ht_##T##_get(HashTable_##T* self, int keylen, const char* key) {\
    int hashed = hash(key, keylen) % self->capacity; \
    return &(self->values[hashed]); \
  } \
  \
  int ht_##T##_finalize(HashTable_##T* self) {\
    free(self->keys); \
    free(self->values); \
    free(self); \
    return 0; \
  }

// Exemplo de função de hash simples
int hash(const char* s, int len) {
    int bitload1 = (((int) s[0] << 24) | ((int) s[1] << 16));
    if (len == 2)
      return bitload1 | (len << 2);
    int bitload2 = ((int) s[len - 2] << 8) | ((int) s[len - 1]);
    int mid = len >> 1;
    return bitload1 | bitload2 | (int) s[mid - 1] | len;
}

// Teste
int main() {
    HASH_TABLE(int)

    HashTable_int* ht = ht_int_new(10);
    ht_int_insert(ht, 3, "abc", 42);
    
    int* value = ht_int_get(ht, 3, "abc");
    if (value != NULL) {
        printf("Value: %d\n", *value);
    } else {
        printf("Key not found\n");
    }

    ht_int_finalize(ht);
    return 0;
}
