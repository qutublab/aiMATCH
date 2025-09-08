# Generative AI for Biomedical Decisions

*April 29, 2025*<br>
*Instructed by Sambit Panda, Ph.D. (University of Texas at San Antonio MATRIX AI Consortium)
and Christian Cruz (University of Texas at San Antonio MATRIX AI Consortium)*

Slides: https://github.com/qutublab/MATCH/releases/download/Generative-ai-workshop/Generative.AI.for.Biomedical.Decisions.pptx

Code: https://github.com/qutublab/MATCH/releases/download/Generative-ai-workshop/genai-biomedical-code.ipynb

## How to Run the Code

1. Install [Python](https://www.python.org/downloads/) and (recommended) install [Anaconda](https://www.anaconda.com/).

2. (recommended) [Create and initiate a new Conda environment](https://docs.conda.io/projects/conda/en/stable/user-guide/tasks/manage-environments.html).

3. Install `jupyter`

```sh
conda install jupyter

# or

pip install jupyter
```

4. Run the Jupyter notebook server

```sh
jupyter notebook
```

Now navigate to the server (by default `http://localhost:8888/`) and open the `dicb-aim-ahead-genai.ipynb` file.

Prior to running the notebook, you will need an [OpenAI API Key](https://platform.openai.com/docs/guides/production-best-practices/api-keys). Please store this in a `.env` file using the variable `OPENAI_API_KEY`.

> Note: You should not commit your `.env` file or it will expose secrets that will allow others to control access to your various AI and authentication provider accounts.

## Acknowledgements

**NIH AIM AHEAD DICB MATCH Workshop**: Generative AI for Biomedical Decisions

**Workshop Speakers**: Sambit Panda, PhD, UTSA and Christian Cruz, UTSA

**Code Author**: Sambit Panda, PhD, UTSA

**Organizer**: Mignon Dumanjog, M.S., UTSA

**MATCH PI**: Amina A. Qutub, PhD
