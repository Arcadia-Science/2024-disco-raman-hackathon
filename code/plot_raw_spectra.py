import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap


def plot_spectra(
    spectra,
    dataframe,
    samples_ordered,
    colors=None,
    figsize=(7, 10),
    filename=None,
):
    """Make a figure displaying the raw Raman spectra of a collection
    of samples.

    Each subplot contains the measured spectra of an individual sample
    with the average spectrum for that sample plotted in bold.
    Samples can be ordered via the `order` and `ascending` parameters.
    For instance, the beer spectra can be plotted by ABV (%) by

    >>> plot_spectra(beer_spectra, beer_dataframe, order="ABV (%)")

    Parameters
    ----------
    spectra : dict
        Mapping of sample name and acquisition time to numpy array
        of the measured Raman spectrum.
    dataframe : pd.DataFrame
        Dataframe with information regarding the samples in `spectra`.
    samples_ordered : list
        List of names of samples to order the subplots.
    colors : list (optional)
        List of colors to define the colormap for the plot.
    figsize : 2-tuple
        Figure size (width, height) of the plot.
    filename : Path
        Filename for saving the figure.
    """
    # create figure
    fig, axes = plt.subplots(
        nrows=len(dataframe),
        figsize=figsize,
    )

    # define a colormap
    if colors is None:
        # default to [aegean, amber, seaweed]
        colors = ["#5088C5", "#F28360", "#3B9886"]
    
    cmap = LinearSegmentedColormap.from_list(
        name="",
        colors=colors,
        N=len(dataframe),
    )

    # loop through spectra
    for i, sample_name in enumerate(samples_ordered):

        # get the spectra for each sample as an array
        sample_spectra = np.array(
            [v for k, v in spectra.items() if k[0] == sample_name]
        )

        # set matplotlib axis
        ax = axes[i]
        # set color from colormap
        color = cmap(i)

        # plot each spectrum semi-transparently and the mean
        # spectrum in bold
        [ax.plot(spectrum, color=color, alpha=0.3) for spectrum in sample_spectra]
        ax.plot(sample_spectra.mean(axis=0), color=color, lw=2)

        # axis-level aesthetics
        title = f"{sample_name}"
        ax.set_title(title, loc="left", y=0.5, va="center", ha="right", fontsize=10)
        ax.get_yaxis().set_ticks([])
        ax.spines["top"].set_visible(False)
        ax.spines["left"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.grid(ls=":")

    # figure-level aesthetics
    fig.subplots_adjust(hspace=0)
    # save figure
    if filename is not None:
        plt.savefig(filename, dpi=144)
