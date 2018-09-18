# include <vector>
# include <iostream>
# include <iomanip>
# include <vector>

struct Point{
  double x;
  double y;
  bool problematic;
};

class Line {
public:
  Line(int length);
	void addPoint(Point p);
	void addPoint(double x, double y, bool problematic);
	int removeIntersections();
	int size();
	std::vector<double> getX();
	std::vector<double> getY();
	std::vector<bool> getProblematic();
private:
	std::vector<Point> m_Points;
	int _removeIntersect(std::vector<Point>::size_type err_point);
	bool _getLineIntersection(int start, int end, Point *i_p);
	void _smoothLine(int start, int end, Point int_point);
	unsigned _nChooseK(unsigned n, unsigned k);
};
