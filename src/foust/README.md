# Foust

Foust is an optimization pass for quantum compilers, written in Coalton, and based on [Intel's PCOAST](https://arxiv.org/abs/2305.10966). 

Foust takes advantage of properties of the Clifford group as the _normalizer_ of the Pauli group by representing all non-Clifford gates in terms of tensor products of Pauli operators or an anticommuting pair thereof. A preserving Foust guarantees that the final quantum state of the output circuit is equivalent to that of the input circuit. A releasing Foust applies more aggressive optimizations while only guaranteeing that measurement statistics are preserved.

## Getting Started

First, [install Coalton](https://github.com/coalton-lang/coalton). Then execute the following command to install other dependencies and load Foust:

``` lisp
(ql:quickload "cl-quil/foust")
```

To run the tests, execute:

``` lisp
(asdf:test-system "cl-quil/foust")
```

## Method

### Hermitian Paulis

Foust will express every gate that is not a single- or two-qubit Clifford gate in terms of one or two Hermitian Paulis. Here, the term Hermitian Pauli refers to any unitary of the form

```math
 \mathbf{P}=\pm P_{n-1}\otimes P_{n-2}\cdots\otimes P_2\otimes P_1\otimes P_0,
 ```

 where $`P_i\in\{I,X,Y,Z\}`$ and $`n`$ describes the number of qubits spanned by the relevant Hilbert space. More concretely, the Hermitian Paulis make up the subgroup of the Pauli group whose elements have a phase of $`\pm 1`$.

 The group operation the closes the subgroup of Hermitian Paulis is a special product defined as,

 ```math
 \left(\mathbf{P}_1,\mathbf{P}_2\right)=
 \begin{cases}
\mathbf{P}_1\mathbf{P}_2 & [\mathbf{P}_1,\mathbf{P}_2]=0\\
-i\mathbf{P}_1\mathbf{P}_2 & \{\mathbf{P}_1,\mathbf{P}_2\}=0
 \end{cases},
 ```

where $`\mathbf{P}_1`$ and $`\mathbf{P}_2`$ are elements of the subgroup of Hermitian Paulis and the operators $`[\cdot,\cdot]`$ and $`\{\cdot,\cdot\}`$ are the commutator and anti-commutator respectively. This definition is complete, as all members of the Pauli group, and, by extension, of the subgroup of Hermitian Paulis, either commute or anti-commute with the others.

### Clifford Frames

All gates which are Clifford gates will be expressed by Foust in terms of Clifford Frames [1]. Clifford Frames are also called stabilizer tableaus or sometimes Pauli tableaus, and they arise from the fact that the Clifford group is the normalizer of the Pauli group, and also of the subgroup of Hermitian Paulis. Equivalently,

```math
\mathbf{U}^\dag\mathbf{P}\mathbf{U}=\mathbf{P}',
```

where $`\mathbf{U}`$ is an element of the Clifford group and $`\mathbf{P}`$ and $`\mathbf{P}'`$ are Hermitian Paulis. Since the Hermitian Paulis are spanned by the set of single-qubit Pauli operators $`\{Z_i, X_i\mid0\le i < n\}`$, then every member $`\mathbf{U}`$ of the Clifford group is uniquely determined by the map,

```math
i\mapsto\left(\mathbf{U}^\dag Z_i\mathbf{U},\mathbf{U}^\dag X_i\mathbf{U}\right),
```
where, once again, $`n`$ describes the number of qubits spanned by the relevant Hilbert space. This map encodes the effect of "pushing" a Clifford operator past a Pauli operator. I.e., if a Clifford operator appears upstream by a Pauli operator, e.g., $`\mathbf{P}\mathbf{U}`$, then the map above describes how to transform $`\mathbf{P}\to\mathbf{P}'`$ such that $`\mathbf{P}\mathbf{U}=\mathbf{U}\mathbf{P}'`$. Foust likes to push Clifford operators downstream, and, therefore, this representation is ideal for efficiency.

### The Gate Set

Foust will interpret all elements of a circuit as one of six elements [1]. These elements include single-axis and bi-axial rotations, preparations, measurements, Clifford gates (Frames), and classical assignments.

Single axis rotations are defined in terms of a single Hermitian Pauli as,

$$\operatorname{Rot}\left(\mathbf{P},\theta\right)=\exp\left(-i\frac{\theta}{2}\mathbf{P}\right).$$

Bi-axial rotations are defined in terms of a pair of anti-commuting Hermitian Paulis as,
$$\operatorname{Rot}_2\left(\mathbf{P},\mathbf{Q},\theta,\varphi\right)=\exp\left(-i\frac{\theta}{2}\left(\cos(\varphi)\mathbf{P}+\sin(\varphi)\mathbf{Q}\right)\right).$$

Measurements are defined in terms of a single Hermitian Pauli as 
$$\operatorname{Meas}\left(\mathbf{P}\rightarrow c\right),$$ 
collapsing a quantum state to the $`+1`$ or $`-1`$ eigenstate of the operator $`\mathbf{P}`$ and storing $`0`$ or $`1`$ in the classical variable $`c`$, respectively.

Preparations are defined in terms of a pair of anti-commuting Hermitian Paulis as,
$$\operatorname{Prep}\left(\mathbf{P},\mathbf{Q}\right),$$ 
equivalent to the sequence,
$$\operatorname{Meas}\left(\mathbf{P}\rightarrow c\right);\text{ if } c=1\text{, then apply }\operatorname{Rot}\left(\mathbf{P},\pi\right);\text{ discard }c.$$

Frames have already been described, and classical assignments are nothing more than a series of classical instructions, manipulating the stored values of classical binary variables.

### Two-Qubit Entangling Gates (TQEs)

The Clifford gates in a Fousted circuit include arbitary single-qubit Clifford gates, of which there are twenty-four, and two-qubit entangling gates. These two-qubit entangling gates (TQEs) are extensions of the gates of the same name presented in [1], extended with insights from [3] to generalize to arbitrary architectures. TQEs, as presented in [1], are generalized $`\operatorname{CNOT}`$ gates, defined by two non-identity Pauli operators, $`P_i`$ and $`P_j`$. A TQE is interpretted accordingly as the operation $`P_j`$ controlled on the qubits being in a negative eigenstate of the operator $`P_i`$, and, symmetrically, as the operation $`P_i`$ controlled on the qubits being in a negative eigenstate of the operator $`P_j`$. Accordingly, the gate $`\operatorname{CNOT}_{ij}`$ is equivalent to to the gate $`\operatorname{TQE}Z_iX_j`$. Altogether, there are nine TQEs of this form.

Foust extends this notion of TQEs with a boolean, `True` or `False`, answering the question "then swap?" There are eighteen TQEs of this form. Any two-qubit Clifford gate capable of producing entanglement is conjugable to one of these eighteen gates by at most two single-qubit Clifford gates [3]. This extension is necessary for Foust to optimize to architectures which perform $`\operatorname{ISWAP}`$ operations, or similar operations, which are not trivially conjugable to other entangling gates including $`\operatorname{CNOT}`$ and $`\operatorname{CZ}`$ gates.

### Foust Graphs

With few exceptions, Foust's optimizations are accomplished by compiling a circuit to a graph and back to a circuit. These foust graphs have several notable properties. First, they consist only of the elements described in the preceding section. Next, with few exceptions, they store only a single frame and a single set of assignments, and these will always be pushed downstream to the end of the graph. Lastly, edges are drawn from upstream to downstream nodes if and only if the nodes do not commute with one another.

### The Search Algorithm

Most of the computation during the optimization occurs in the process of compiling from a graph back to a circuit. This is because the resulting circuit should have only gates which operate on a single qubit, with the exception of two-qubit entangling gates. In order to accomplish this compilation, elements have to be reduced to ones considered "free". For example, a single qubit rotation is "free", but a two-qubit rotation has a cost of $`1`$ because it will take exactly one two-qubit entangling gate to reduce it to a "free" rotation. The algorithm for reducing these various elements is described by the following simplified pseudo-code [2].

```lisp
(loop (process-and-remove-free-elements elements)
      (reduce-cost-of-cheapest-element elements))
```

Very simply, all free elements are processed, and then the cheapest element remaining is reduced. This is repeated until all elements are processed. Here, a processed element can mean a single-qubit gate added to a circuit or a row of a Clifford Frame interpretted as a single-qubit Clifford operator. It also can mean a single-qubit measurement added to a circuit, chosen to span remaining commuting terminal measurements in a subroutine of the aggressive releasing Foust optimization.

## Benchmarking

To run benchmarks, execute the following commands (assuming all dependencies have been installed):

``` lisp
> (asdf:load-system "cl-quil-benchmarking/foust")
> (cl-quil-benchmarking.foust:cl-foust-benchmark-qasm-suite)
```

The following benchmark was generated on a MacBook Pro, 2020, with a 2.3 GHz Quad-Core Intel Core i7 processor and 16 GB 3744 MHz of memory. It was completed on September 24, 2024, with SBCL, and with Coalton compiled in Release mode. The `timeout` for each of the five steps per file was set to two minutes.

`WITHOUT FOUST` corresponds to the sequence, `parse file -> compile to chip`.

`WITH NAIVE FOUST` corresponds to the sequence, `parse file -> preserving Foust without knowledge of connectivity -> compile to chip`.

`WITH CHIP-AWARE FOUST` corresponds to the sequence, `parse file -> preserving Foust with knowledge of connectivity -> compile to chip`.

Under `VALIDATION`, the `Matrix?` column includes the results of computing the unitary matrices from the compilations with and without Foust and checking their equality, and the `Amplitudes?` column includes the results of simulating the compiled circuits with the intial state $\mid0\rangle$ and checking the equality of the resulting amplitudes.

```
┌─────────────────┬───────────────────────────┬───────────────────────────┬───────────────────────────┬──────────────────────┐
│                 │       WITHOUT FOUST       │     WITH NAIVE FOUST      │   WITH CHIP-AWARE FOUST   │      VALIDATION      │
├─────────────────┼───────────────────────────┼───────────────────────────┼───────────────────────────┼──────────────────────┤
│       NAME      │ TIME (s)  SWAPS  2Q DEPTH │ TIME (s)  SWAPS  2Q DEPTH │ TIME (s)  SWAPS  2Q DEPTH │ Matrix?  Amplitudes? │
├─────────────────┼───────────────────────────┼───────────────────────────┼───────────────────────────┼──────────────────────┤
│ 0410184_169     │ 5.434993     84       254 │ 6.819166     72       262 │ 7.308465     75       277 │ ???????      YES     │
│ 3_17_13         │ 0.584091      9        30 │ 0.531976      5        17 │ 0.703073      5        17 │   YES        YES     │
│ 4_49_16         │ 3.499261     70       205 │ 3.519753     31       123 │ 3.629836     29       120 │ ???????      YES     │
│ 4gt10-v1_81     │ 2.941124     48       150 │ 2.528865     21        86 │ 2.840503     22        91 │ ???????      YES     │
│ 4gt11_82        │ 1.076145     10        38 │ 0.625058      2        17 │ 0.799723      2        19 │   YES        YES     │
│ 4gt11_83        │ 0.710385      9        25 │ 0.766203      3        14 │ 0.950857      3        14 │   YES        YES     │
│ 4gt11_84        │ 0.534709      5        16 │ 0.577918      4        12 │ 0.751102      4        12 │   YES        YES     │
│ 4gt12-v0_86     │ 3.065752     82       218 │ 4.959093     43       181 │ 4.650787     40       168 │ ???????      YES     │
│ 4gt12-v0_87     │ 2.797707     81       209 │ 4.323994     33       143 │ 4.549477     36       147 │   YES        YES     │
│ 4gt12-v0_88     │ 3.512212     64       190 │ 3.070276     22        93 │ 3.307328     24       100 │   YES        YES     │
│ 4gt12-v1_89     │ 4.561983     80       216 │ 3.821413     29       116 │ 3.731989     26       122 │   YES        YES     │
│ 4gt13-v1_93     │ 1.724873     23        75 │ 1.356688      6        34 │ 1.595813      9        40 │   YES        YES     │
│ 4gt13_90        │ 2.314276     33       106 │ 2.044489     12        51 │ 2.167138     15        53 │ ???????      YES     │
│ 4gt13_91        │ 1.957302     30        87 │ 1.941258      9        41 │ 2.360064     13        53 │ ???????      YES     │
│ 4gt13_92        │ 1.718212     18        62 │ 1.535821      8        36 │ 1.651441      8        34 │ ???????      YES     │
│ 4gt4-v0_72      │ 5.329608     81       242 │ 3.860548     32       136 │ 4.455689     33       141 │ ???????      YES     │
│ 4gt4-v0_73      │ 6.107811    146       397 │ 7.191699     50       230 │ 6.914364     52       210 │ ???????      YES     │
│ 4gt4-v0_78      │ 3.824298     80       223 │ 4.396927     34       149 │ 4.107442     35       150 │ ???????      YES     │
│ 4gt4-v0_79      │ 3.333978     84       211 │ 4.569832     41       162 │ 4.269905     41       150 │ ???????      YES     │
│ 4gt4-v0_80      │ 3.087508     65       168 │ 2.794643     25       105 │ 2.700779     29       101 │   YES        YES     │
│ 4gt4-v1_74      │ 4.213719    100       273 │ 4.629115     49       188 │ 4.561379     49       176 │ ???????      YES     │
│ 4gt5_75         │ 1.894525     25        73 │ 1.789174     12        55 │ 1.838855     12        55 │ ???????      YES     │
│ 4gt5_76         │ 1.590932     27        81 │ 1.619844     11        40 │ 1.832533     10        43 │   YES        YES     │
│ 4gt5_77         │ 2.459703     45       129 │ 2.597585     16        71 │ 2.233986     13        62 │   YES        YES     │
│ 4mod5-bdd_287   │ 1.335535     27        64 │ 1.710778     12        48 │ 2.009443     11        50 │   YES        YES     │
│ 4mod5-v0_18     │ 1.511314     21        62 │ 0.967416      4        19 │ 1.136701      4        19 │ ???????      YES     │
│ 4mod5-v0_19     │ 0.838548     12        30 │ 0.524911      2        16 │ 0.761018      3        17 │ ???????      YES     │
│ 4mod5-v0_20     │ 0.891199      6        20 │ 0.558662      1         8 │ 0.719657      1         8 │   YES        YES     │
│ 4mod5-v1_22     │ 0.534318      6        15 │ 0.571506      1         9 │ 0.742357      1         9 │   YES        YES     │
│ 4mod5-v1_23     │ 1.565726     26        66 │ 1.220043      9        38 │ 1.411457      7        29 │ ???????      YES     │
│ 4mod5-v1_24     │ 0.750352     11        31 │ 0.756183      6        19 │ 0.939305      6        19 │   YES        YES     │
│ 4mod7-v0_94     │ 2.595309     51       143 │ 2.644835     18        82 │ 2.712855     17        79 │ ???????      YES     │
│ 4mod7-v1_96     │ 3.188251     47       147 │ 2.834817     22        99 │ 2.688376     22        78 │ ???????      YES     │
│ 9symml_195      │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ C17_204         │ 7.871588    160       435 │ 7.41188d     70       290 │ 7.427954     72       287 │ ???????      YES     │
│ adr4_197        │ 78.90072   1279      3631 │ 85.71614    784      2867 │ 86.59163    752      2844 │ ???????      YES     │
│ aj-e11_165      │ 2.118564     53       144 │ 2.814403     20        87 │ 3.016477     19        93 │ ???????      YES     │
│ alu-bdd_288     │ 1.825783     31        93 │ 2.239364     13        62 │ 2.268767      9        53 │ ???????      YES     │
│ alu-v0_26       │ 1.832608     28        88 │ 1.766567     11        51 │ 2.126935     13        56 │ ???????      YES     │
│ alu-v0_27       │ 1.003823     10        29 │ 1.012316      6        26 │ 1.184181      6        26 │   YES        YES     │
│ alu-v1_28       │ 0.887704     11        33 │ 1.166676      8        31 │ 1.338864      8        31 │ ???????      YES     │
│ alu-v1_29       │ 0.90041d     11        34 │ 1.037104      7        27 │ 1.219797      7        27 │   YES        YES     │
│ alu-v2_30       │ 8.240795    167       482 │ 10.14291     97       397 │ 9.712682     96       377 │ ???????      YES     │
│ alu-v2_31       │ 6.323501    129       425 │ 5.170911     47       198 │ 5.616206     47       213 │ ???????      YES     │
│ alu-v2_32       │ 2.970368     48       156 │ 2.533304     24        83 │ 2.775326     24        86 │ ???????      YES     │
│ alu-v2_33       │ 0.883954      9        32 │ 1.455583      8        31 │ 1.429287     12        39 │   YES        YES     │
│ alu-v3_34       │ 1.383926     18        48 │ 1.113164      8        35 │ 1.289934      8        33 │   YES        YES     │
│ alu-v3_35       │ 0.757969     11        33 │ 0.891819      4        23 │ 1.077609      6        26 │   YES        YES     │
│ alu-v4_36       │ 2.204023     40        99 │ 1.934736     17        65 │ 1.993952     16        58 │ ???????      YES     │
│ alu-v4_37       │ 0.944076      8        30 │ 0.882757      4        23 │ 1.071947      6        26 │   YES        YES     │
│ clip_206        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ cm152a_212      │ 24.60386    456      1302 │ 22.02956    242       886 │ 20.75268    214       812 │ ???????      YES     │
│ cm42a_207       │ 35.81942    621      1749 │ 31.53656    303      1161 │ 31.58101    333      1199 │ ???????      YES     │
│ cm82a_208       │ 9.546753    234       635 │ 10.84165    114       443 │ 10.87445    122       448 │ ???????      YES     │
│ cm85a_209       │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ cnt3-5_179      │ 5.097061     62       211 │ 5.412739     63       230 │ 4.792542     63       205 │ ???????      YES     │
│ cnt3-5_180      │ 10.78354    200       581 │ 11.05494    111       412 │ 11.63988    127       439 │ ???????      YES     │
│ co14_215        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ con1_216        │ 18.48109    335       994 │ 17.97559    176       691 │ 19.04326    182       734 │ ???????      YES     │
│ cycle10_2_110   │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ dc1_220         │ 34.76177    699      1916 │ 31.83980    297      1161 │ 31.94121    314      1194 │ ???????      YES     │
│ dc2_222         │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ decod24-bdd_294 │ 1.262529     19        67 │ 1.656618     11        47 │ 1.684842     10        40 │ ???????      YES     │
│ decod24-enable_1│ 4.700182    116       311 │ 5.868329     60       242 │ 5.605282     51       226 │ ???????      YES     │
│ decod24-v0_38   │ 0.783475     12        41 │ 0.971348      7        28 │ 1.311501      7        29 │   YES        YES     │
│ decod24-v1_41   │ 1.750348     25        77 │ 1.737498     10        45 │ 2.045407     14        56 │ ???????      YES     │
│ decod24-v2_43   │ 1.080869     17        43 │ 0.999158      7        27 │ 1.186024      7        27 │   YES        YES     │
│ decod24-v3_45   │ 3.253719     45       147 │ 2.495069     23        85 │ 2.571311     19        84 │ ???????      YES     │
│ dist_223        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ ex-1_166        │ 0.438912      6        15 │ 0.459111      2        10 │ 0.655752      2        10 │   YES        YES     │
│ ex1_226         │ 0.653424      2        11 │ 0.578156      1         6 │ 0.745807      1         6 │   YES        YES     │
│ ex2_227         │ 11.21455    228       619 │ 10.13860     91       359 │ 9.763664     85       361 │ ???????      YES     │
│ ex3_229         │ 6.432813    130       379 │ 5.369566     65       236 │ 6.431119     66       261 │ ???????      YES     │
│ f2_232          │ 20.70101    414      1203 │ 19.26006    191       748 │ 19.93907    201       773 │ ???????      YES     │
│ graycode6_47    │ 0.572593      0         5 │ 0.579443      0         5 │ 0.76122d      0         5 │   YES        YES     │
│ ground_state_est│ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ ham15_107       │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ ham3_102        │ 0.45215d      6        15 │ 0.460556      2        12 │ 0.634868      2        12 │   YES        YES     │
│ ham7_104        │ 5.793778    107       344 │ 5.366379     52       203 │ 5.366162     54       217 │   YES        YES     │
│ hwb4_49         │ 3.968223     79       221 │ 3.32519d     32       128 │ 3.752171     26       127 │   YES        YES     │
│ hwb5_53         │ 22.14278    464      1342 │ 23.26987    206       808 │ 24.06706    203       838 │ ???????      YES     │
│ hwb6_56         │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ hwb7_59         │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ hwb8_113        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ hwb9_119        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ inc_237         │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ ising_model_10  │ 2.820152      0        90 │ 3.906171     27       146 │ 3.497601     19       120 │   YES        YES     │
│ ising_model_13  │ 3.651733      0       120 │ 3.943744     17       130 │ 4.069509     17       130 │ ???????      YES     │
│ ising_model_16  │ 4.520813      0       150 │ 5.813972     42       203 │ 6.003973     42       203 │ ???????      YES     │
│ life_238        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ majority_239    │ 10.48284    227       592 │ 9.896523     92       373 │ 9.368513     89       356 │   YES        YES     │
│ max46_240       │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ miller_11       │ 0.829021     18        42 │ 0.606117      8        27 │ 0.781736      8        27 │   YES        YES     │
│ mini-alu_167    │ 5.030934     89       271 │ 4.9064d0     53       203 │ 5.238117     47       189 │ ???????      YES     │
│ mini_alu_305    │ 3.348382     67       176 │ 3.945122     35       144 │ 3.844715     34       141 │   YES        YES     │
│ misex1_241      │ 116.8705   1754      5074 │ 114.9082    963      3519 │ 112.4831    933      3408 │ ???????      YES     │
│ mlp4_245        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ mod10_171       │ 3.719048     75       221 │ 4.190276     38       148 │ 4.341344     39       146 │ ???????      YES     │
│ mod10_176       │ 2.860989     55       161 │ 2.956164     24        96 │ 3.459286     26       112 │ ???????      YES     │
│ mod5adder_127   │ 10.43395    193       556 │ 9.104099     97       387 │ 9.546436     91       360 │ ???????      YES     │
│ mod5d1_63       │ 0.682147      7        22 │ 0.782448      2        13 │ 0.945044      2        13 │   YES        YES     │
│ mod5d2_64       │ 1.579111     19        60 │ 1.595403      7        36 │ 1.622918      7        36 │ ???????      YES     │
│ mod5mils_65     │ 0.894053     12        36 │ 0.538619      2        16 │ 0.957025      3        20 │   YES        YES     │
│ mod8-10_177     │ 6.5774d0    154       426 │ 7.021142     63       261 │ 7.385633     71       282 │ ???????      YES     │
│ mod8-10_178     │ 5.588831    122       324 │ 5.412794     59       220 │ 5.98028d     63       240 │ ???????      YES     │
│ one-two-three-v0│ 4.582656    100       266 │ 4.761999     43       171 │ 4.522265     39       161 │ ???????      YES     │
│ one-two-three-v0│ 3.058477     52       141 │ 2.33818d     19        81 │ 2.691727     16        74 │ ???????      YES     │
│ one-two-three-v1│ 2.543129     42       125 │ 2.376811     20        78 │ 2.768463     26        89 │ ???????      YES     │
│ one-two-three-v2│ 1.463773     19        65 │ 1.79979d     12        52 │ 1.575904      9        36 │   YES        YES     │
│ one-two-three-v3│ 1.608093     17        69 │ 1.30194d      7        32 │ 1.432287      9        37 │   YES        YES     │
│ plus63mod4096_16│ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ plus63mod8192_16│ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ pm1_249         │ 34.34187    644      1821 │ 30.45709    325      1198 │ 29.08482    281      1105 │ ???????      YES     │
│ qft_10          │ 3.427826     27        75 │ 0.35742d      0         0 │ 0.543182      0         0 │   YES        YES     │
│ qft_16          │ 7.862491     73       219 │ 0.38389d      0         0 │ 0.546227      0         0 │ ???????      YES     │
│ radd_250        │ 73.01250   1185      3336 │ 74.20134    691      2543 │ 69.60619    657      2454 │ ???????      YES     │
│ rd32-v0_66      │ 0.685994     13        27 │ 0.651639      4        12 │ 0.853011      5        13 │   YES        YES     │
│ rd32-v1_68      │ 0.682358     13        27 │ 0.6757d0      4        12 │ 0.854469      5        13 │   YES        YES     │
│ rd32_270        │ 1.350219     29        77 │ 0.945657      5        19 │ 1.259529      6        26 │   YES        YES     │
│ rd53_130        │ 17.72819    357      1017 │ 17.72945    170       690 │ 17.98085    165       685 │ ???????      YES     │
│ rd53_131        │ 7.840166    142       438 │ 5.330062     44       206 │ 6.230953     59       231 │ ???????      YES     │
│ rd53_133        │ 11.17963    188       568 │ 7.938754     61       265 │ 8.945099     71       308 │ ???????      YES     │
│ rd53_135        │ 5.189258    101       313 │ 5.297009     45       191 │ 4.829433     45       176 │   YES        YES     │
│ rd53_138        │ 2.433397     42       126 │ 2.859891     27       106 │ 3.394739     25       110 │ ???????      YES     │
│ rd53_251        │ 23.45042    448      1262 │ 21.92869    191       742 │ 19.58051    176       706 │ ???????      YES     │
│ rd53_311        │ 6.427317    109       323 │ 6.296362     56       209 │ 5.833245     52       209 │ ???????      YES     │
│ rd73_140        │ 3.901325     87       229 │ 5.311533     54       206 │ 5.681796     49       200 │   YES        YES     │
│ rd73_252        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ rd84_142        │ 8.619611    138       436 │ 9.378002    100       373 │ 9.750892    102       382 │ ???????      YES     │
│ rd84_253        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ root_255        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ sao2_257        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ sf_274          │ 11.89924    266       740 │ 8.409004     76       335 │ 9.546996     91       367 │   YES        YES     │
│ sf_276          │ 12.11126    262       724 │ 10.80923    132       497 │ 10.70163    132       484 │ ???????      YES     │
│ sqn_258         │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ sqrt8_260       │ 71.67304   1160      3376 │ 66.17784    624      2327 │ 69.4444d    700      2527 │ ???????      YES     │
│ squar5_261      │ 40.7386d    738      2053 │ 41.33606    404      1465 │ 39.24379    383      1440 │ ???????      YES     │
│ square_root_7   │ TIMEOUT!  ?????  ???????? │ 115.3819    868      3400 │ 118.0053    923      3686 │ ???????  ??????????? │
│ sym10_262       │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ sym6_145        │ 75.15256   1396      3853 │ 72.87222    587      2336 │ 72.04769    601      2327 │ ???????      YES     │
│ sym6_316        │ 5.531204    125       338 │ 5.457592     47       181 │ 6.057509     59       221 │ ???????      YES     │
│ sym9_146        │ 6.624973    115       335 │ 7.319453     80       302 │ 7.004103     77       293 │ ???????      YES     │
│ sym9_148        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ sym9_193        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ sys6-v0_111     │ 4.00192d     73       227 │ 4.605682     36       163 │ 5.900882     44       193 │   YES        YES     │
│ urf1_149        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ urf1_278        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ urf2_152        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ urf2_277        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ urf3_155        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ urf3_279        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ urf4_187        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ urf5_158        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ urf5_280        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ urf6_160        │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ TIMEOUT!  ?????  ???????? │ ???????  ??????????? │
│ wim_266         │ 18.31915    344       987 │ 16.29483    162       654 │ 16.87495    154       643 │ ???????      YES     │
│ xor5_254        │ 0.707601      2        11 │ 0.586785      1         6 │ 0.779968      1         6 │   YES        YES     │
│ z4_268          │ 65.14839   1142      3243 │ 66.39029    606      2260 │ 67.48103    593      2262 │ ???????      YES     │
└─────────────────┴───────────────────────────┴───────────────────────────┴───────────────────────────┴──────────────────────┘
```

## References

[1] J. Paykin, A. T. Schmitz, M. Ibrahim, X.-C. Wu, and A. Y. Matsuura, Pcoast: A Pauli-based Quantum Circuit Optimization Framework (Extended Version) (2023), [arXiv:2305.10966v2](https://arxiv.org/abs/2305.10966v2) [quant-ph].

[2] A. T. Schmitz, M. Ibrahim, N. P. D. Sawaya, G. G. Guerreschi, J. Paykin, X.-C. Wu, and A. Y. Matsuura, Optimization at the Interface of Unitary and  Non-unitary Quantum Operations in PCOAST (2023), [arXiv:2305.09843](https://arxiv.org/abs/2305.09843) [quant-ph].

[3] D. Grier and L. Schaeffer, The Classification of Clifford Gates over Qubits, Quantum 6, 734 (2022), [arXiv:1603.03999v4](https://arxiv.org/abs/1603.03999v4) [quant-ph].
