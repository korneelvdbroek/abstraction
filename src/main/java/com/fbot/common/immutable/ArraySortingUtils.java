package com.fbot.common.immutable;

import java.util.Comparator;
import java.util.PriorityQueue;

/**
 * Copyright (C) 6/9/2017 - REstore NV
 */
public class ArraySortingUtils {

    // head of PQ is the least element
    // if we need the kth smallest, then we need to keep a heap with the largest of the k smallest in the PQ (i.e. a max heap)
    // so our comparator is the inverse of the natural ordering ">"
    public static <T> PriorityQueue<T> partialSort(T[] array, int k, Comparator<? super T> comparator) {
        PriorityQueue<T> pq = new PriorityQueue<>(k, comparator);

        // load up the PQ
        for(int i = 0; i < k && i < array.length ; i++) {
            pq.offer(array[i]);
        }

        //
        for(int i = k; i < array.length ; i++) {
            if(comparator.compare(array[i], pq.peek()) > 0) {
                pq.poll();
                pq.offer(array[i]);
            }
        }

        return pq;
    }

}
