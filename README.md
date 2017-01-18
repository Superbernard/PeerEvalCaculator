# PeerEvalCaculator
A GUI application to automatically calculate and output peer evaluation score of students in BIOL1121.

Peer evaluation was added into the grading items since Fall2015 in the course BIOL1121 Introductary to Zoology Lab. The evaluation is done through a survey. Typical questions in the survey include "What is the first and last name of your first group member?", "Assign a score for this group member." etc. Based on the answer of these questions, peer evaluation score of each student is computed by simply averaging scores he or she received from all evaluators (other group members in the team). However, the online learning system we use cannot automatically compute this score for us. As a result, it became a time-consuming task, especially for a course with around 25 sections and more than 800 students enrolled. Every time, GTAs (graduate teaching assistant) of this course have to spend much time and effort manually going through the surveys, recording names of students being evaluated and doing calculations in Excel. The aim of this project is to develop an automatic output tool that can automatically extract target information from the input data and compute and export peer evaluation scores. Hopefully, it can free our GTAs from this repetitious work.

The major challenge of this project is that, not all student evaluators would provide the exact names of their teammates in the survey. I saw nicknames, misspellings, missing first or last names very frequently. So it is important to match the non-exact name inputs with the precise names on the class roster. To do this, Levenshtein distance is used in the script to determine the best match of each name input.   
