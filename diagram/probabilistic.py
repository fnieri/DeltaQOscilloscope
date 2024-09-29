class ProbabilisticOperator:
    def __init__(self, following_components):
        self.following_components_and_probabilities = following_components
        if not self.check_probabilities_sum():
            raise ValueError("The sum of probabilities must equal 1.")

    def check_probabilities_sum(self):
        total_prob = sum(self.following_components_and_probabilities.values())
        return abs(total_prob - 1) < 1e-6

    def calculate_dq(self):
        pdfs = []

        for k,v in self.following_components_and_probabilities.items():
            pass