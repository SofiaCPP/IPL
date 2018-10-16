#include <bits/stdc++.h>
int N = 0;
int a[750001] = {0};
/*Problem Secrect DAA*/
// lalallala
int partition(int l, int r)
{
    int randIdx = l + rand() % (r - l);

    int pivot = a[randIdx];

    std::swap(a[l], a[randIdx]);
    int ind = l;

    for (int i = l + 1; i < r; ++i)
    {
        if (a[i] <= pivot)
        {
            ind++;
            std::swap(a[i], a[ind]);
        }
    }

    std::swap(a[l], a[ind]);
    return ind;
}

void quickSort(int l, int r)
{
    if (l < r)
    {
        int idx = partition(l, r);
        quickSort(l, idx);
        quickSort(idx + 1, r);
    }
}

int binarySearch(int k)
{
    int middle, left = 0, right = N - 1;
    if (k < a[left])
        return (-1);
    if (k == a[left])
        return (left + 1);
    if (k > a[right])
        return (-1);

    while (right - left > 1)
    {
        middle = (left + right) / 2;
        if (k <= a[middle])
            right = middle;
        else
            left = middle;
    }
    return ((right <= N - 1 && a[right] == k) ? (right + 1) : (-1));
}

int main()
{
    srand(time(0));
    scanf("%d", &N);
    for (int i = 0; i < N; ++i)
    {
        scanf("%d ", &a[i]);
    }
    quickSort(0, N);

    int tmp = 0;
    scanf("%d", &tmp);
    while (tmp != 0)
    {
        printf("%d\n", binarySearch(tmp));
        scanf("%d", &tmp);
    }
    return 0;
}
