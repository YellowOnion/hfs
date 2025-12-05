# Proof of concept CoW key-value store

The general design philosophy is inspired by bcachefs.

We abuse CoW principles to maintain consistency and performance, all data is immutable once written.

* Bucket based allocation
  * currently a static 256kB this should be bigger than most SSD erase block sizes.
  * Each bucket can only have one kind of data in it (Journal, BTree, User, Free etc)
  * we pre allocate buckets for writes, this is very important to keep enough buckets allocated for btree updates.
  * we only append to a bucket, we never do out of order writes
  * a bucket is only for avoiding write locks, we
* self-hosting BTree
 * all metadata is stored in the btree
 * The BTree itself should be resilient, can we recover without the journal? Yes, the journal is only there for performance, it should also act as an independent source of truth when debugging.
* Single file implementation
 * You can use a raw block device for maximum performance
 
 
#### Notes
Other projects like lmdb or acid-state leave something to be desired, lmdb is C and no one maintains a Haskell friendly binding, while acid-state seems quite primitive, no automatic recovery, overly verbose on-disk fromat.
I'm doing this mostly for self learning, I needed a big project to code in Haskell
I also eventually want to use to store data for a game like Minecraft or Noita.

#### TODO
* look in to using async, io_uring
* testing
