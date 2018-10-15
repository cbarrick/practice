# CSIP 2018-10-15

Sign in: https://tinyurl.com/csip-contact



## Linked List Review

A doubly linked list consists of nodes which contain three parts: a value, a pointer to the next node, and a pointer to the previous node. The following figure demonstrates a list with four nodes:

```
+------------+    +------------+    +------------+    +------------+
| Value:  10 |    | Value: 2   |    | Value: 42  |    | Value: 99  |
+------------+    +------------+    +------------+    +------------+
| Next  ========> | Next  ========> | Next  ========> | Next  NULL |
+------------+    +------------+    +------------+    +------------+
| Prev: NULL | <========  Prev | <========  Prev | <========  Prev |
+------------+    +------------+    +------------+    +------------+
```

A doubly linked list in Java might look something like this:

```java
class Node {
	int value;
	Node next;
	Node prev;
}
```
























## Problem 1

Given the first node of a doubly linked list, sort the list using [insertion sort](https://en.wikipedia.org/wiki/Insertion_sort).

Here's a quick explanation of insertion sort. Starting at position 2, for each node, slide the node back in the list until the value in the previous node is less than or equal to the value in the current node.

See [this graphic](https://upload.wikimedia.org/wikipedia/commons/0/0f/Insertion-sort-example-300px.gif) for a visual explanation.








































## Problem 2

Given the first nodes of two sorted doubly linked lists, merge them into one.
