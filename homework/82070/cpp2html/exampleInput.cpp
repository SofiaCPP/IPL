#include <iostream>
#include <cmath>
using namespace std;

struct point {
	double x;
	double y;
	void set_point(double, double);
	void print();
};

int main() {
	const int maxPoints = 32;
	int counterPoints = 0, counterTriangle = 0, counterPokeball = 0, outside;
	point arrPoints[maxPoints];
	for (int i = 0; i < maxPoints; ++i) {
		double x, y;
		cin >> x >> y;
		if (cin.good()) {
			arrPoints[i].set_point(x, y);
			counterPoints++;
		}
		else {
			cin.clear();
			cin.ignore(numeric_limits<streamsize>::max(), '\n');
			break;
		}
	}
	for (int i = 0; i < counterPoints; ++i) {
		bool insideTriangle = false, insidePokeball = false;
		point triangleVertex, lineCrossOx, lineCrossOy, pokeballCenter;
		triangleVertex.set_point(2, 2);
		lineCrossOx.set_point(6, 0);
		lineCrossOy.set_point(0, 6);
		pokeballCenter.set_point(-9, 0);
		if (arrPoints[i].x >= triangleVertex.x
			&& arrPoints[i].y >= triangleVertex.y
			&& arrPoints[i].x * lineCrossOy.y + arrPoints[i].y * lineCrossOx.x <= lineCrossOx.x * lineCrossOy.y) {
			insideTriangle = true;
			counterTriangle++;
		}
		else {
			const double smallCircleR = 1, largeCircleR = 3;
			double distanceX = abs(arrPoints[i].x - pokeballCenter.x);
			double distanceY = abs(arrPoints[i].y - pokeballCenter.y);
			double distanceFromPokeballCenter = sqrt(distanceX * distanceX + distanceY * distanceY);
			if (arrPoints[i].x >= pokeballCenter.x - largeCircleR
				&& arrPoints[i].x <= pokeballCenter.x + largeCircleR
				&& arrPoints[i].y >= pokeballCenter.y
				&& arrPoints[i].y <= pokeballCenter.y + largeCircleR
				&& distanceFromPokeballCenter >= smallCircleR && distanceFromPokeballCenter <= largeCircleR) {
				insidePokeball = true;
				counterPokeball++;
			}
		}
		arrPoints[i].print();
		if (insideTriangle) {
			cout << "inside, " << i + 1 << " in the list of points (It is inside the triangle)." << endl;
		}
		else if (insidePokeball) {
			cout << "inside, " << i + 1 << " in the list of points (It is inside the pokeball)." << endl;
		}
		else {
			cout << "outside, " << i + 1 << " in the list of points" << endl;
		}
	}
	outside = counterPoints - (counterTriangle + counterPokeball);
	cout << counterTriangle << " points inside triangle, " << counterPokeball << " points inside pokeball, " << outside << " points outside out of " << counterPoints << " points." << endl;
	return 0;
}

void point::set_point(double x, double y) {
	this->x = x;
	this->y = y;
}

void point::print() {
	cout << "<" << this->x << ", " << this->y << ">: ";
}