#include <iostream>


class Polynomial {
    struct Term {
        int coeff = 0;
        int exp = 0;
        Term *nxt;

        Term(int coefficient, int exponent, Term *next = nullptr) : coeff(coefficient), exp(exponent), nxt(next) {}
    };

    Term *head = nullptr;  // biggest degree

    void plus_minus_eq(const Polynomial &other, bool plus) {
        if (!other.head)
            return;

        auto operation = plus ? [](int &a, int b) { a += b; } : [](int &a, int b) { a -= b; };
        int factor = plus ? 1 : -1;

        Term *curr1 = head = new Term(0, 0, head);
        Term *curr2 = other.head;

        while (curr1->nxt && curr2)
            if (curr1->nxt->exp == curr2->exp) {
                operation(curr1->nxt->coeff, curr2->coeff);
                if (!curr1->nxt->coeff) {
                    Term *tmp = curr1->nxt;
                    curr1->nxt = tmp->nxt;
                    delete tmp;
                } else
                    curr1 = curr1->nxt;
                curr2 = curr2->nxt;
            } else if (curr1->nxt->exp < curr2->exp) {
                curr1->nxt = new Term(factor * curr2->coeff, curr2->exp, curr1->nxt);
                curr1 = curr1->nxt;
                curr2 = curr2->nxt;
            } else
                curr1 = curr1->nxt;

        for (; curr2; curr2 = curr2->nxt)
            curr1 = curr1->nxt = new Term(factor * curr2->coeff, curr2->exp);

        curr1 = head;
        head = head->nxt;
        delete curr1;
    }

public:
    Polynomial &add_term(int coeff, int exp) {
        if (!coeff)
            return *this;

        if (!head || head->exp < exp)
            head = new Term(coeff, exp, head);
        else if (head->exp > exp) {
            Term *curr = head;
            for (; curr->nxt && curr->nxt->exp > exp; curr = curr->nxt);

            if (curr->nxt && curr->nxt->exp == exp) {
                curr->nxt->coeff += coeff;
                if (!curr->nxt->coeff) {
                    Term *tmp = curr->nxt;
                    curr->nxt = tmp->nxt;
                    delete tmp;
                }
            } else
                curr->nxt = new Term(coeff, exp, curr->nxt);
        } else {
            head->coeff += coeff;
            if (!head->coeff) {
                Term *tmp = head;
                head = tmp->nxt;
                delete tmp;
            }
        }

        return *this;
    }

    ~Polynomial() {
        multiply_by_term(0, 0);
    }

    friend std::ostream &operator<<(std::ostream &os, const Polynomial &polynomial) {
        if (!polynomial.head)
            return os << '0';

        Term *curr = polynomial.head;
        os << curr->coeff << "x^" << curr->exp;
        curr = curr->nxt;
        for (; curr; curr = curr->nxt)
            os << (curr->coeff < 0 ? " - " : " + ") << std::abs(curr->coeff) << "x^" << curr->exp;

        return os;
    }

    Polynomial &operator+=(const Polynomial &other) {
        this->plus_minus_eq(other, true);
        return *this;
    }

    Polynomial operator+(const Polynomial &other) const {
        Polynomial sum;
        sum += *this;
        sum += other;
        return sum;
    }

    Polynomial &operator-=(const Polynomial &other) {
        this->plus_minus_eq(other, false);
        return *this;
    }

    Polynomial operator-(const Polynomial &other) const {
        Polynomial diff;
        diff += *this;
        diff -= other;
        return diff;
    }

    Polynomial &multiply_by_term(int coefficient, int exp) {
        if (coefficient) {
            for (Term *curr = head; curr; curr = curr->nxt) {
                curr->coeff *= coefficient;
                curr->exp += exp;
            }
            return *this;
        }

        Term *tmp;
        while (head) {
            tmp = head;
            head = head->nxt;
            delete tmp;
        }

        return *this;
    }

    Polynomial operator*(const Polynomial &other) const {
        Polynomial product;

        for (Term *curr = head; curr; curr = curr->nxt) {
            Polynomial term;
            term += other;
            term.multiply_by_term(curr->coeff, curr->exp);
            product += term;
        }

        return product;
    }

    Polynomial &operator*=(const Polynomial &other) {
        Polynomial product;
        product += *this * other;
        this->multiply_by_term(0, 0);
        *this += product;
        return *this;
    }

    [[nodiscard]] auto div(const Polynomial &divisor) const {
        struct PolyDiv_t {
            Polynomial quot;
            Polynomial rem;
        } poly_div_t;

        Term *curr = poly_div_t.quot.head = new Term(0, 0);
        poly_div_t.rem += *this;

        while (poly_div_t.rem.head && poly_div_t.rem.head->exp >= divisor.head->exp) {
            int coeff = poly_div_t.rem.head->coeff / divisor.head->coeff;
            int exp = poly_div_t.rem.head->exp - divisor.head->exp;

            curr = curr->nxt = new Term(coeff, exp);
            Polynomial term;
            term += divisor;
            term.multiply_by_term(coeff, exp);
            poly_div_t.rem -= term;
        }

        curr = poly_div_t.quot.head;
        poly_div_t.quot.head = curr->nxt;
        delete curr;

        return poly_div_t;
    }

    Polynomial operator/(const Polynomial &other) const {
        Polynomial quotient;
        quotient += this->div(other).quot;
        return quotient;
    }

    Polynomial operator%(const Polynomial &other) const {
        Polynomial remainder;
        remainder += this->div(other).rem;
        return remainder;
    }

    [[nodiscard]] Polynomial operator()(const Polynomial &other) const {
        Polynomial result;

        for (Term *curr = head; curr; curr = curr->nxt) {
            Polynomial term;
            term.add_term(curr->coeff, 0);

            for (int i = 0; i < curr->exp; ++i)
                term *= other;

            result += term;
        }

        return result;
    }

    [[nodiscard]] int compose_eval(int num) const {
        Polynomial p, result;
        p.add_term(num, 0);
        result += (*this)(p);
        return result.head ? result.head->coeff : 0;
    }

    [[nodiscard]] int operator()(int num) const {
        Polynomial divisor;
        divisor.add_term(1, 1).add_term(-num, 0);

        Polynomial remainder;
        remainder += *this % divisor;

        return !remainder.head ? 0 : remainder.head->coeff;
    }

};


int main() {
    Polynomial p1;
    p1.add_term(2, 4).add_term(-3, 3).add_term(4, 2).add_term(-5, 1).add_term(6, 0);
//    p1.add_term(1, 2).add_term(-3, 1).add_term(1, 0);
    std::cout << "P1: " << p1 << "\n";

    Polynomial p2;
    p2.add_term(1, 2).add_term(-3, 1).add_term(1, 0);
//    p2.add_term(1, 1);
    std::cout << "P2: " << p2 << "\n\n";

    std::cout << "P1 + P2 = " << p1 + p2 << "\nP1 - P2 = " << p1 - p2 << "\n\n";

    std::cout << "P1 * P2 = " << p1 * p2 << "\n";

    std::cout << "P1 / P2 = " << p1 / p2 << "\nP1 % P2 = " << p1 % p2 << "\n\n";

    std::cout << "P1(2) = " << p1(2) << "\n\n";

    std::cout << "P1 o P2: " << p1(p2) << "\n\n";

    std::cout << "P2(1) = " << p2.compose_eval(1);
}
