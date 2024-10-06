import numpy as np
import matplotlib.pyplot as plt

# Step 1: Create a CDF that is 0 until x = 1, and jumps to 1 at x = 1
x = np.linspace(0, 2, 500)  # Define the range of x values
cdf = np.zeros_like(x)       # Initialize CDF with zeros
cdf[x >= 1] = 1.0            # Set the CDF to 1 for x >= 1

# Step 2: Multiply the CDF by 0.95
cdf_scaled = 0.8 * cdf

# Step 3: Plot the original and scaled CDF
plt.figure(figsize=(8, 6))
plt.plot(x, cdf, label="Original CDF", linestyle="--", color="blue")
plt.plot(x, cdf_scaled, label="Scaled CDF (0.95 * CDF)", color="red")

# Adding labels and title
plt.title("Original and Scaled CDF")
plt.xlabel("x")
plt.ylabel("CDF(x)")
plt.legend()
plt.grid(True)
plt.show()
