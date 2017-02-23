using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace PolishNotation
{
    class Parser
    {
        public string message;
        public Dictionary<string, List<double>> arguments = new Dictionary<string, List<double>>();
        public List<double> answers = new List<double>();

        //演算状態．
        //0 => 変数なし演算
        //1 => 変数登録済みだが数式で未使用
        //2 => 変数が使用された場合
        private int calculator_state_ = 0;
        private string[] formula_;
        private Stack<double> numbers_stack_ = new Stack<double>();

        //変数テーブル     [変数名] { [初期値] [限度値] [現在値] }
        private Dictionary<string, double[]> variables_table_ = new Dictionary<string, double[]>();

        //予約語           [operator] [operand counts]
        private Dictionary<string, int> reserved_words_ = new Dictionary<string, int>()
        {
            {"+",2 }, {"-",2 }, {"*",2 }, {"/",2 }, { "abs",1}, {"pow",2 }, {"sqrt",1}, {"sin",1 }, {"cos",1 }, {"log",2 },{"PI",0 }, {"E",0}
        };

        private string[][] graph_ = new string[11][];

        //概形の出力
        public void printGraph()
        {
            if (calculator_state_ == 2 && arguments.Keys.Count() == 1)
            {
                foreach (var r in graph_)
                {
                    foreach (var c in r) Console.Write("{0}", c);
                    Console.WriteLine();
                }
            }
        }

        //入力数式の解析と演算
        public int analysisFormula(string ar_input, string ar_graph_option)
        {
            if (separateFormula(ar_input) != 0 ||
                calculateFormula() != 0 ||
                generateGraph(ar_graph_option) != 0)
            {
                return -1;
            }

            return 0;
        }

        private int separateFormula(string ar_input)
        {
            string[] _separated_input;

            //入力数式を数式部，定義部に分ける
            _separated_input = ar_input.Split(',');

            if (_separated_input.Length > 1)
            {
                if (registrateVariable(_separated_input) != 0)
                {
                    return -1;
                }
            }

            formula_ = _separated_input[0].Split('_');

            return 0;
        }
        private int calculateFormula()
        {
            int _devide_count =
                (calculator_state_ == 1) ? 2 * graph_.Length : 0;

            for (int i = 0; i <= _devide_count; ++i)
            {
                foreach (var s in formula_)
                {
                    double _r;
                    //数値
                    if (double.TryParse(s, out _r))
                    {
                        numbers_stack_.Push(double.Parse(s));
                    }
                    //予約語
                    else if (reserved_words_.Keys.Contains(s))
                    {
                        if (numbers_stack_.Count < reserved_words_[s])
                        {
                            message = "Please input require arguments.";
                            return -1;
                        }
                        else if (Operators(s) != 0)
                        {
                            message = "Can not divide by 0.";
                            return -1;
                        }
                    }
                    //変数
                    else if (Regex.IsMatch(s, "[a-zA-Z]+[a-zA-Z0-9_]*"))
                    {
                        if (variables_table_.ContainsKey(s))
                        {
                            calculator_state_ = 2;
                            registrateArguments(s);
                            numbers_stack_.Push(variables_table_[s][2]);
                        }
                        else
                        {
                            message = "Please input domain of \"" + s + "\".";
                            return -1;
                        }
                    }
                    //上記以外
                    else
                    {
                        string tmp = (s == "") ? "[space]" : s;
                        message = tmp + " is invalid term.";

                        return -1;
                    }
                }

                if (numbers_stack_.Count != 0)
                {
                    answers.Add(numbers_stack_.Peek());
                    numbers_stack_.Pop();

                    //もしスタックに数字が残っていたならばエラー
                    if (numbers_stack_.Count != 0)
                    {
                        message = "Invalid formula.";
                        return -1;
                    }
                }
                updateArguments(_devide_count);
            }

            answers = (calculator_state_ == 1) ? answers.Distinct().ToList() : answers;

            return 0;
        }
        private int generateGraph(string ar_graph_option)
        {
            if (calculator_state_ == 2 && arguments.Keys.Count() == 1)
            {
                for (int i = 0; i < graph_.Length; ++i)
                    graph_[i] = Enumerable.Repeat("　", graph_.Length).ToArray();

                double _y_interval = Math.Abs(answers.Max() - answers.Min()) / graph_.Length;
                generateCurves(_y_interval);
                return generateAxis(_y_interval, ar_graph_option);
            }
            return 0;
        }

        //演算子
        private int Operators(string ar_op)
        {
            switch (ar_op)
            {
                case "+":
                    twoOperandFunction(Add);
                    break;
                case "-":
                    twoOperandFunction(Sub);
                    break;
                case "*":
                    twoOperandFunction(Mul);
                    break;
                case "/":
                    if (numbers_stack_.Peek() == 0)
                        return -1;
                    twoOperandFunction(Div);
                    break;

                case "abs":
                    oneOperandFunction(Math.Abs);
                    break;
                case "pow":
                    twoOperandFunction(Math.Pow);
                    break;
                case "sqrt":
                    oneOperandFunction(Math.Sqrt);
                    break;
                case "sin":
                    oneOperandFunction(Math.Sin);
                    break;
                case "cos":
                    oneOperandFunction(Math.Cos);
                    break;
                case "log":
                    twoOperandFunction(Math.Log);
                    break;

                case "PI":
                    numbers_stack_.Push(Math.PI);
                    break;
                case "E":
                    numbers_stack_.Push(Math.E);
                    break;
            }
            return 0;
        }
        private void oneOperandFunction(Func<double, double> ar_func)
        {
            double _num;

            _num = numbers_stack_.Peek();
            numbers_stack_.Pop();

            numbers_stack_.Push(ar_func(_num));
        }
        private void twoOperandFunction(Func<double, double, double> ar_func)
        {
            double _numA, _numB;

            _numB = numbers_stack_.Peek(); numbers_stack_.Pop();
            _numA = numbers_stack_.Peek(); numbers_stack_.Pop();

            if (ar_func == Math.Log)
                numbers_stack_.Push(Math.Log(_numB) / Math.Log(_numA));
            else
                numbers_stack_.Push(ar_func(_numA, _numB));
        }

        private double Add(double ar_numA, double ar_numB)
        {
            return ar_numA + ar_numB;
        }
        private double Sub(double ar_numA, double ar_numB)
        {
            return Add(ar_numA, -ar_numB);
        }
        private double Mul(double ar_numA, double ar_numB)
        {
            return ar_numA * ar_numB;
        }
        private double Div(double ar_numA, double ar_numB)
        {
            return ar_numA / ar_numB;
        }

        //変数
        private int registrateVariable(string[] ar_separated_input)
        {
            string[] _separated_domains, _domain;
            double _r;

            _separated_domains = ar_separated_input.Skip(1).Take(ar_separated_input.Length).ToArray();
            foreach (var d in _separated_domains)
            {
                _domain = d.Split('_');
                //変数は[変数名_数字_数字]の形か？
                if (_domain.Length != 3)
                {
                    message = "Invalid domain : Length == 3.";
                    return -1;
                }
                //予約語に含まれていないか？
                else if (reserved_words_.Keys.Contains(_domain[0]))
                {
                    message = "Invalid domain : Variable name must NOT be the reversed word.";
                    return -1;
                }
                //既に登録されていないか？
                else if (variables_table_.ContainsKey(_domain[0]))
                {
                    message = "Invalid domain : Variable is already registrated.";
                    return -1;
                }
                //変数の登録（変数は型どおりか？，変数の後に数字が続くか？）
                else if (Regex.IsMatch(_domain[0], "[a-zA-Z]+[a-zA-Z0-9_]*")
                    && double.TryParse(_domain[1], out _r) && double.TryParse(_domain[2], out _r))
                {
                    variables_table_.Add(_domain[0], new[] { double.Parse(_domain[1]), double.Parse(_domain[2]), double.Parse(_domain[1]) });
                }
                else
                {
                    message = "Invalid domain : [variable]_[number]_[number].";
                    return -1;
                }
            }
            calculator_state_ = 1;
            return 0;
        }
        private void registrateArguments(string ar_str)
        {
            if (arguments.ContainsKey(ar_str)) arguments[ar_str].Add(variables_table_[ar_str][2]);
            else
            {
                arguments.Add(ar_str, new List<double>());
                arguments[ar_str].Add(variables_table_[ar_str][2]);
            }
        }
        private void updateArguments(int ar_devide_count)
        {
            foreach (var a in arguments)
                variables_table_[a.Key][2] +=
                    (variables_table_[a.Key][1] - variables_table_[a.Key][0]) / ar_devide_count;
        }

        //概形
        private void generateCurves(double ar_y_interval)
        {
            double _threshold = ar_y_interval / 3;
            for (int ci = 0; ci < graph_.Length; ++ci)
            {
                if (ar_y_interval != 0)
                {
                    double _a = answers[2 * ci];
                    double _b = answers[2 * ci + 1];
                    double _c = answers[2 * ci + 2];

                    int _answer_index = graph_.Length - 1 - (int)((_b - answers.Min()) / ar_y_interval);
                    _answer_index = (_answer_index < 0) ? 0 : _answer_index;

                    if (Math.Abs(_a - _c) <= _threshold)
                    {
                        if ((_b - _a) > _threshold && (_b - _c) > _threshold) graph_[_answer_index][ci] = "∧";
                        else if ((_b - _a) < -_threshold && (_b - _c) < -_threshold) graph_[_answer_index][ci] = "∨";
                        else
                        {
                            double _tmp = (_b - (answers.Min() + (graph_.Length - 1 - _answer_index) * ar_y_interval)) / ar_y_interval;
                            if (_tmp >= 0.66) graph_[_answer_index][ci] = "￣";
                            else if (_tmp >= 0.33) graph_[_answer_index][ci] = "―";
                            else graph_[_answer_index][ci] = "＿";
                        }
                    }
                    else if (_a < _c) graph_[_answer_index][ci] = "／";
                    else if (_a > _c) graph_[_answer_index][ci] = "＼";
                }
            }

        }
        private int generateAxis(double ar_y_interval, string ar_graph_option)
        {
            string[] _options = { "", "x", "y", "xy" };

            if (!_options.Contains(ar_graph_option))
            {
                message = "Please input valid option";
                return -1;
            }
            if (ar_graph_option.Contains("y"))
            {
                double _x_interval =
                    Math.Abs(arguments.ElementAt(0).Value.Max() - arguments.ElementAt(0).Value.Min()) / graph_.Length;

                if (arguments.ElementAt(0).Value.Any(e => e <= _x_interval / 2 && e >= -_x_interval / 2))
                {
                    double tmp = arguments.ElementAt(0).Value.First(_a => _a <= _x_interval / 2 && _a >= -_x_interval / 2);

                    int _y_axis_index = (int)((tmp - arguments.ElementAt(0).Value.Min()) / _x_interval);
                    _y_axis_index = (_y_axis_index < 0) ? 0 : _y_axis_index;

                    for (int ri = 0; ri < graph_.Length; ++ri)
                        graph_[ri][_y_axis_index] = "｜";
                }
            }
            if (ar_graph_option.Contains("x"))
            {
                if (answers.Any(_a => _a <= ar_y_interval / 2 && _a >= -ar_y_interval / 2))
                {
                    double tmp = answers.First(_a => _a <= ar_y_interval / 2 && _a >= -ar_y_interval / 2);

                    int _x_axis_index = graph_.Length - 1 - (int)((tmp - answers.Min()) / ar_y_interval);
                    _x_axis_index = (_x_axis_index < 0) ? 0 : _x_axis_index;

                    graph_[_x_axis_index] = Enumerable.Repeat("―", graph_.Length).ToArray();
                }
            }

            return 0;
        }

        private void printValuesOfVariables(string ar_variable)
        {
            Console.WriteLine("Variable name：" + ar_variable +
                ", From:" + variables_table_[ar_variable][0] +
                ", To:" + variables_table_[ar_variable][1] +
                ", Value:" + variables_table_[ar_variable][2]);
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            Parser parser = new Parser();
            string formula, option;

            formula = Console.ReadLine();
            option = Console.ReadLine();

            if (parser.analysisFormula(formula, option) != 0)
                Console.WriteLine(parser.message);
            else
            {
                if (parser.answers.Count == 1)
                    Console.WriteLine("answer:" + parser.answers[0]);
                else
                    parser.printGraph();
            }
        }
    }
}
