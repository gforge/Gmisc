#include "Rcpp.h"
#include <Gmisc.h>


Line::Line(int length){
  m_Points.reserve(length);
}

void Line::addPoint(Point p){
	m_Points.push_back(p);
}

void Line::addPoint(double x, double y, bool problematic){
	Point p = { x, y, problematic };
	m_Points.push_back(p);
}

int Line::removeIntersections(){
	int intersections_no = 0;
	for (std::vector<Point>::size_type err_i = m_Points.size() - 1; err_i > 0; err_i--){
		if (m_Points[err_i].problematic){
			int pos = _removeIntersect(err_i);
			if (pos >= 0){
				// We need to reset to where we started the smoothening
				err_i = pos;
				intersections_no++;
			}
			else {
				// If this no intersection was found we need to set the point to be equal to the previous
				// in order to preserve the number of elements
				m_Points[err_i] = m_Points[err_i + 1];
			}
		}
	}
	return(intersections_no);
}

int Line::size(){
	return(m_Points.size());
}

std::vector<double> Line::getX(){
	std::vector<double> x(m_Points.size());
	for (std::vector<Point>::size_type i = 0; i < m_Points.size(); i++)
		x[i] = m_Points[i].x;

	return(x);
}

std::vector<double> Line::getY(){
	std::vector<double> y(m_Points.size());
	for (std::vector<Point>::size_type i = 0; i < m_Points.size(); i++)
		y[i] = m_Points[i].y;

	return(y);
}

std::vector<bool> Line::getProblematic(){
	std::vector<bool> prblm(m_Points.size());
	for (std::vector<Point>::size_type i = 0; i < m_Points.size(); i++)
		prblm[i] = m_Points[i].problematic;

	return(prblm);
}

int Line::_removeIntersect(std::vector<Point>::size_type err_point){
	for (
	    std::vector<Point>::size_type end = err_point;
      end < m_Points.size() - 2; 
      end++
  ) {
		for (
		    size_t start = err_point - 1;
        start > 1;
        start--
    ) {
		  Point int_point = { -1, -1, false };
			bool found = _getLineIntersection(start, end, &int_point);

			if (found){
				_smoothLine(start, end, int_point);
				return(start);
			}
		}
	}
	return(-1);
}

bool Line::_getLineIntersection(
    int start,
    int end,
	  Point *i_p
) {
  // TODO: the points order is reversed due to old design pattern, 
  // probably doesn't matter but it needs to be checked
  Point p0 = m_Points[end];
  Point p1 = m_Points[end + 1];
  Point p2 = m_Points[start];
  Point p3 = m_Points[start - 1];
  
	double s02_x, s02_y, s10_x, s10_y, s32_x, s32_y, s_numer, t_numer, denom, t;
	s10_x = p1.x - p0.x;
	s10_y = p1.y - p0.y;
	s32_x = p3.x - p2.x;
	s32_y = p3.y - p2.y;


	denom = s10_x * s32_y - s32_x * s10_y;
	if (fabs(denom) < FLT_EPSILON)
		return false; // Collinear

	bool denomPositive = denom > 0;

	s02_x = p0.x - p2.x;
	s02_y = p0.y - p2.y;
	s_numer = s10_x * s02_y - s10_y * s02_x;
	if ((s_numer < 0) == denomPositive)
		return false; // No collision

	t_numer = s32_x * s02_y - s32_y * s02_x;
	if ((t_numer < 0) == denomPositive)
		return false; // No collision

	if (((s_numer > denom) == denomPositive) || ((t_numer > denom) == denomPositive))
		return false; // No collision

	// Collision detected
	t = t_numer / denom;
	i_p->x = p0.x + (t * s10_x);
	i_p->y = p0.y + (t * s10_y);

	return true;
}

// From: https://stackoverflow.com/questions/9330915/number-of-combinations-n-choose-r-in-c
unsigned Line::_nChooseK(unsigned n, unsigned k){
	if (k > n) return 0;
	if (k * 2 > n) k = n - k;
	if (k == 0) return 1;

	unsigned result = n;
	for (unsigned i = 2; i <= k; ++i) {
		result *= (n - i + 1);
		result /= i;
	}
	return result;
}

void Line::_smoothLine(int start, int end, Point int_point){
  unsigned length_out = end - start + 2;
	double t = 0.0;
	std::vector<Point> ctrl_points(4);
	ctrl_points[0] = m_Points[start - 1];
	ctrl_points[1] = int_point;
	ctrl_points[2] = int_point;
	ctrl_points[3] = m_Points[end + 1];
	for (unsigned i = 0; i < length_out; i++){
		if (i + 1 == length_out)
			t = 1.0;
		else
			t += 1.0 / double(length_out);

		m_Points[start + i].x = 0;
		m_Points[start + i].y = 0;

		int n = ctrl_points.size() - 1;
		for (unsigned ii = 0; ii < ctrl_points.size(); ii++){
			double b_ii_n = _nChooseK(n, ii) *
				std::pow(1 - t, n - ii) *
				std::pow(t, ii);
			m_Points[start + i].x +=
				b_ii_n *
				ctrl_points[ii].x;
			m_Points[start + i].y +=
				b_ii_n *
				ctrl_points[ii].y;
		}
		m_Points[start + i].problematic = false;

	}
}
