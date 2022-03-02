import pandas as pd
from ranking.models.model import Model


def evaluate(ranks: pd.Series):
    total = len(ranks)
    top_one = len(ranks[ranks == 1])
    top_ten = len(ranks[(ranks != 0) & (ranks <= 10)])
    mrr = ranks.apply(to_reciprocal_rank).mean()
    result = {'Total': [total],
              'TopOne Total:': [top_one],
              'TopOne in %': [top_one / total],
              'TopTen Total:': [top_ten],
              'TopTen in %': [top_ten / total],
              'MRR': [mrr]}
    return pd.DataFrame(result)


def get_rank(expected_result, result, top_n=10):
    top_n_results = result[:top_n]
    if expected_result in top_n_results:
        return top_n_results.index(expected_result) + 1
    else:
        return 0


def to_reciprocal_rank(rank):
    return 1 / rank if rank else 0
