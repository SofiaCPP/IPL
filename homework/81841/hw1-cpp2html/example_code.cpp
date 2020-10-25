#include <bits/stdc++.h>
using namespace std;

struct tree
{
    bool active=true;
    int toProp=0;
    int sum=0;
    pair<int, int> range;
    tree *left=nullptr; 
    tree *right=nullptr;

    int size(pair<int, int> range){return range.second-range.first+1;}
    int size(){return size(this->range);}

    tree(pair<int, int> range)
    {
        this->range=range;

        if(size()>1)
        {
            int mid=(range.first+range.second)/2;
            left=new tree({range.first, mid});
            right=new tree({mid+1, range.second});
        }
    }

    void reset(int upTo)
    {
        sum=0;
        toProp=0;
        if(upTo<range.first)
        {
            active=false;
            if(size()!=1)
            {
                left->toProp=0;
                left->sum=0;
                right->sum=0;
                right->toProp=0;
            }
            return;
        }
        else active=true;
        if(size()!=1)
        {
            left->reset(upTo);
            right->reset(upTo);
        }
    }

    void add(const pair<int, int> &inRange, int value=1)
    {
        if(range.second<inRange.first || range.first>inRange.second)return;

        if(range.first>=inRange.first && range.second<=inRange.second)
        {
            sum+=size()*value;
            toProp+=value;
        }
        else
        {
            left->add(inRange, value);
            right->add(inRange, value);
            sum=left->sum+right->sum;
        }
    }

    void prop()
    {
        if(toProp!=0)
        {
            left->add(range, toProp);
            right->add(range, toProp);
            sum=left->sum+right->sum;
            toProp=0;
        }
    }

    int64_t max_sum()
    {
        if(size()==1)return sum;
        else if (active)
        {
            prop();
            return max(left->max_sum(), right->max_sum());
        }
        else return sum;
    }

    int get_sum(const pair<int, int> &inRange)
    {
        if(range.second<inRange.first || range.first>inRange.second)return 0;

        if(range.first>=inRange.first && range.second<=inRange.second)
        {
            return sum;
        }
        else
        {
            prop();
            return left->get_sum(inRange)+right->get_sum(inRange);
        }
    }
};

const double PI=atan(1)*4;

struct point
{
    double x, y;

    point(double x, double y)
    {
        this->x=x;
        this->y=y;
    }
    point():point(0.0, 0.0){}

    bool operator == (const point &p)const
    {
        return x==p.x && y==p.y;
    }
};

point middle(point a, point b)
{
    return point((a.x+b.x)/2, (a.y+b.y)/2);
}

double dist_sq(const point &a, const point &b)
{
    int sA=a.x-b.x;
    int sB=a.y-b.y;
    return sA*sA+sB*sB;
}

double dist(const point &a, const point &b)
{
    return hypot(a.x-b.x, a.y-b.y);
}

double area(double a, double b, double c)
{
    double p=(a+b+c)/2;
    return sqrt(p*(p-a)*(p-b)*(p-c));
}

double angle(point o, point p)
{
    double rt=atan2(p.y-o.y, p.x-o.x);
    if(rt<0)rt+=2*PI;
    return rt*180/PI;
}

double normalize(double angle)
{
    //while(angle>=360)angle-=360;
    angle-=360*(((int)angle)/360);
    while(angle<0)angle+=360;
    return angle;
}

int n, r, smallR;
vector<point> circles;

int64_t max_stacked(const vector<pair<double, double>> &ranges)
{
    if(ranges.size()==0)return 0;
    static vector<pair<int, int>> intRanges;
    intRanges.clear();
    static map<double, int> nums;
    nums.clear();
    //intRanges.reserve(ranges.size());

    for(auto &x : ranges)nums[x.first]=nums[x.second]=-1;
    {int i=0; for(auto &x : nums)x.second=i++;}

    for(auto &x : ranges)intRanges.emplace_back(nums[x.first], nums[x.second]);

    int len=nums.rbegin()->second;
    vector<int> count(len+1, 0);

    static tree tr({0, n*2});
    tr.reset(len);

    for(auto &x : intRanges)
    {
        //for(int i=x.first;i<=x.second;i++)count[i]++;
        tr.add({x.first, x.second}, 1);
    }

    return tr.max_sum();
}

void read()
{
    cin>>n>>r>>smallR;
    circles.resize(n);
    for(auto &x : circles)cin>>x.x>>x.y;
}

void solve()
{
    if(r==smallR)
    {
        if(n==0)puts("0");
        else puts("1");
        return;
    }

    r-=smallR;

    //printf("r: %d\n", r);

    int64_t answer=INT_MIN;

    int diamSq=pow(r*2, 2);
    int rSq=r*r;

    vector<pair<double, double>> ranges;
    ranges.reserve(n*3);

    for(int i=0;i<n;i++)
    {
        ranges.clear();
        for(int j=0;j<n;j++)
        {
            if(j==i)continue;
            point &a=circles[i];
            point &b=circles[j];
            double abSq=dist_sq(a, b);
            if(abSq>diamSq)continue;
            double angle=acos(sqrt(abSq/(4*rSq)))*180/PI;
            double angleAb=::angle(a, b);
            double angle1=normalize(angleAb-angle);
            double angle2=angle1+2*angle;
            ranges.emplace_back(angle1, angle2);
        }

        if(ranges.size()+1<=answer)continue;
        int64_t ms=max_stacked(ranges);
        answer=max(answer, ms+1);
    }

    printf("%lld\n", answer);
}

int main()
{
    /*
    point zero(0, 0);
    cout<<angle(zero, {1, 0})<<endl;
    cout<<angle(zero, {1, 1})<<endl;
    cout<<angle(zero, {0, 1})<<endl;
    cout<<angle(zero, {-1, 1})<<endl;
    cout<<angle(zero, {-1, 0})<<endl;
    cout<<angle(zero, {-1, -1})<<endl;
    cout<<angle(zero, {0, -1})<<endl;
    cout<<angle(zero, {1, -1})<<endl;
    */
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    cin>>t;
    for(int i=0;i<t;i++)
    {
        read();
        solve();
    }
}

/*
2

4 2 1
0 0
0 1
0 2
0 3

8 11 7
0 0
1 4
3 2
5 3
-2 -2
-1 0
4 8
0 5

*/
