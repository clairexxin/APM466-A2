import matplotlib.pyplot as plt
import csv

if __name__ == "__main__":
    filename = "optimal_node_up.csv"
    # filename = "optimal_node_down.csv"

    with open(filename, "r") as csv_file:
        csv_read = csv.DictReader(csv_file)
        pre_key = "X"
        for i, row in enumerate(csv_read):
            for j in range(len(row.keys())):
                y, x = i + 1, j + 1
                if row[list(row.keys())[j]] == "NA":
                    # no data
                    pass
                if row[list(row.keys())[j]] == "0":
                    # 0
                    plt.plot(x, y, 'bo')
                if row[list(row.keys())[j]] == "1":
                    # mark red
                    plt.plot(x, y, 'ro')
        plt.show()