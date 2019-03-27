#ifndef DAGGR_FUNCTION_TRAITS_H
#define DAGGR_FUNCTION_TRAITS_H

/*----- System Includes -----*/

#include <tuple>
#include <string>
#include <functional>
#include <type_traits>

/*----- Type Declarations -----*/

namespace daggr::meta {

  // Struct represents regular void.
  struct none {};
  constexpr none none_v {};

  // Struct represents a non-fatal error in a context where a valid type cannot be instantiated.
  struct nonesuch {
    nonesuch() = delete;
    nonesuch(nonesuch const&) = delete;
    nonesuch(nonesuch&&) = delete;
    ~nonesuch() = delete;
  };

  // Detection idiom implementation.
  namespace detail {
    template <class Default, class AlwaysVoid, template <class...> class Op, class... Args>
    struct detector {
      using value_t = std::false_type;
      using type = Default;
    };
    template <class Default, template <class...> class Op, class... Args>
    struct detector<Default, std::void_t<Op<Args...>>, Op, Args...> {
      using value_t = std::true_type;
      using type = Op<Args...>;
    };
  }

  // Detection idiom.
  template <template <class...> class Op, class... Args>
  using is_detected = typename detail::detector<nonesuch, void, Op, Args...>::value_t;
  template <template <class...> class Op, class... Args>
  constexpr auto is_detected_v = is_detected<Op, Args...>::value;
  template <template <class...> class Op, class... Args>
  using detected_t = typename detail::detector<nonesuch, void, Op, Args...>::type;
  template <class Default, template <class...> class Op, class... Args>
  using detected_or = detail::detector<Default, void, Op, Args...>;
  template <class Default, template <class...> class Op, class... Args>
  using detected_or_t = typename detail::detector<Default, void, Op, Args...>::type;

  // Calculates if a given type is a specialization of a given template.
  template <class T, template <class...> class Template>
  struct is_specialization : std::false_type {};
  template <template <class...> class Specialization, class... Params>
  struct is_specialization<Specialization<Params...>, Specialization> : std::true_type {};
  template <class T, template <class...> class Template>
  constexpr auto is_specialization_v = is_specialization<T, Template>::value;

  // Calculates if T is a specialization of std::tuple.
  template <class T>
  struct is_tuple : is_specialization<T, std::tuple> {};
  template <class T>
  constexpr auto is_tuple_v = is_tuple<T>::value;

  // Returns the first type in a parameter pack.
  template <class... Ts>
  struct first_type {
    using type = nonesuch;
  };
  template <class T, class... Ts>
  struct first_type<T, Ts...> {
    using type = T;
  };
  template <class... Ts>
  using first_type_t = typename first_type<Ts...>::type;

  template <template <class...> class Predicate, class... Ts>
  struct all_of : std::conjunction<Predicate<Ts>...> {};
  template <template <class...> class Predicate, class... Ts>
  constexpr auto all_of_v = all_of<Predicate, Ts...>::value;

  template <template <class...> class Predicate, class... Ts>
  struct any_of : std::disjunction<Predicate<Ts>...> {};
  template <template <class...> class Predicate, class... Ts>
  constexpr auto any_of_v = any_of<Predicate, Ts...>::value;

  template <template <class...> class Predicate, class... Ts>
  struct none_of : std::conjunction<std::negation<Predicate<Ts>>...> {};
  template <template <class...> class Predicate, class... Ts>
  constexpr auto none_of_v = none_of<Predicate, Ts...>::value;

  // Implementation of type list tranformations.
  namespace detail {
    template <template <class...> class Transform, class TypeList, size_t idx, class... Transformed>
    struct transform_type_sequence_impl {
      static constexpr size_t current_idx = idx - 1;
      using current_transform_t = typename Transform<
        std::tuple_element_t<current_idx, TypeList>
      >::type;

      using type =
        typename transform_type_sequence_impl<
          Transform,
          TypeList,
          current_idx,
          current_transform_t,
          Transformed...
        >::type;
    };
    template <template <class...> class Transform, class TypeList, class... Transformed>
    struct transform_type_sequence_impl<Transform, TypeList, 0, Transformed...> {
      static constexpr size_t current_idx = 0;
      using type = std::tuple<Transformed...>;
    };
  }

  // Type list tranformation according to some transform meta-function.
  template <template <class...> class Transform, class... Ts>
  struct transform_type_sequence {
    using type =
      typename detail::transform_type_sequence_impl<Transform, std::tuple<Ts...>, sizeof...(Ts)>::type;
  };
  template <template <class...> class Transform, class... Ts>
  using transform_type_sequence_t = typename transform_type_sequence<Transform, Ts...>::type;

  // Implementation of type list filtering.
  namespace detail {
    template <template <class...> class Predicate, class TypeList, size_t idx, size_t... idxs>
    struct filter_type_sequence_impl {
      static constexpr size_t current_idx = idx - 1;
      using next_filter_t = std::conditional_t<
        Predicate<
          std::tuple_element_t<current_idx, TypeList>
        >::value,
        filter_type_sequence_impl<Predicate, TypeList, current_idx, current_idx, idxs...>,
        filter_type_sequence_impl<Predicate, TypeList, current_idx, idxs...>
      >;

      using type = typename next_filter_t::type;
      using index_type = typename next_filter_t::index_type;
    };
    template <template <class...> class Predicate, class TypeList, size_t... idxs>
    struct filter_type_sequence_impl<Predicate, TypeList, 0, idxs...> {
      static constexpr size_t current_idx = 0;

      using type = std::tuple<
        std::tuple_element_t<idxs, TypeList>...
      >;
      using index_type = std::index_sequence<idxs...>;
    };
  }

  // Type list filtering according to some predicate.
  template <template <class...> class Predicate, class... Ts>
  struct filter_type_sequence {
    using impl_type_t = detail::filter_type_sequence_impl<Predicate, std::tuple<Ts...>, sizeof...(Ts)>;

    using type = typename impl_type_t::type;
    using index_type = typename impl_type_t::index_type;
  };
  template <template <class...> class Predicate, class... Ts>
  using filter_type_sequence_t = typename filter_type_sequence<Predicate, Ts...>::type;
  template <template <class...> class Predicate, class... Ts>
  using filter_type_sequence_idxs_t = typename filter_type_sequence<Predicate, Ts...>::index_type;

  template <template <class...> class Predicate, class T>
  struct filter_tuple {
    using type = T;
    using index_type = std::index_sequence<>;
  };
  template <template <class...> class Predicate, class... Ts>
  struct filter_tuple<Predicate, std::tuple<Ts...>> {
    using type = filter_type_sequence_t<Predicate, Ts...>;
    using index_type = filter_type_sequence_idxs_t<Predicate, Ts...>;
  };

  // Filter meta-function that filters out any instance of meta::none or void.
  template <class Type>
  struct none_filter :
    std::negation<
      std::disjunction<
        std::is_same<
          std::decay_t<Type>,
          none
        >,
        std::is_same<
          std::decay_t<Type>,
          void
        >
      >
    >
  {};

  // Removes none from a type list to allow for regular void operations.
  template <class... Ts>
  struct filter_none : filter_type_sequence<none_filter, Ts...> {};
  template <class... Ts>
  using filter_none_t = typename filter_none<Ts...>::type;
  template <class... Ts>
  using filter_none_idxs_t = typename filter_none<Ts...>::index_type;

  template <class T>
  struct filter_tuple_none : filter_tuple<none_filter, T> {};
  template <class T>
  using filter_tuple_none_t = typename filter_tuple_none<T>::type;
  template <class T>
  using filter_tuple_none_idxs_t = typename filter_tuple_none<T>::index_type;

  // is_invocable implementation.
  namespace detail {
    template <class Func, class... Args>
    std::is_invocable<Func, Args...> is_invocable_impl(Func&&, std::tuple<Args...>);
    template <class Func, class... Args>
    std::is_nothrow_invocable<Func, Args...> is_nothrow_invocable_impl(Func&&, std::tuple<Args...>);
    template <class Ret, class Func, class... Args>
    std::is_invocable_r<Ret, Func, Args...> is_invocable_r_impl(Func&&, std::tuple<Args...>);
    template <class Ret, class Func, class... Args>
    std::is_nothrow_invocable_r<Ret, Func, Args...> is_nothrow_invocable_r_impl(Func&&, std::tuple<Args...>);
    template <class Func, class... Args>
    using is_invocable_t = decltype(
      is_invocable_impl(
        std::declval<Func>(),
        std::declval<filter_none_t<Args...>>()
      )
    );
    template <class Func, class... Args>
    using is_nothrow_invocable_t = decltype(
      is_nothrow_invocable_impl(
        std::declval<Func>(),
        std::declval<filter_none_t<Args...>>()
      )
    );
    template <class Func, class... Args>
    using is_invocable_r_t = decltype(
      is_invocable_r_impl(
        std::declval<Func>(),
        std::declval<filter_none_t<Args...>>()
      )
    );
    template <class Func, class... Args>
    using is_nothrow_invocable_r_t = decltype(
      is_nothrow_invocable_r_impl(
        std::declval<Func>(),
        std::declval<filter_none_t<Args...>>()
      )
    );
  }

  // Calculates whether a given function object is callable with the sequence of
  // provided arguments, filtering out any instances of none.
  template <class Func, class... Args>
  struct is_invocable : detail::is_invocable_t<Func, Args...> {
    using canonical_type = std::bool_constant<detail::is_invocable_t<Func, Args...>::value>;
  };
  template <class Func, class... Args>
  constexpr auto is_invocable_v = is_invocable<Func, Args...>::value;

  // Calculates whether a given function object could throw an exception when called with
  // the sequence of provided arguments, filtering out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Func, class... Args>
  struct is_nothrow_invocable : detail::is_nothrow_invocable_t<Func, Args...> {
    using canonical_type = std::bool_constant<detail::is_nothrow_invocable_t<Func, Args...>::value>;
  };
  template <class Func, class... Args>
  constexpr auto is_nothrow_invocable_v = is_nothrow_invocable<Func, Args...>::value;

  // Calculates whether a given function object returns a type implicitly convertible to
  // the given return type when called with the sequence of provided arguments, filtering
  // out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Ret, class Func, class... Args>
  struct is_invocable_r : detail::is_invocable_r_t<Ret, Func, Args...> {
    using canonical_type = std::bool_constant<detail::is_invocable_r_t<Ret, Func, Args...>::value>;
  };
  template <class Ret, class Func, class... Args>
  constexpr auto is_invocable_r_v = is_invocable_r<Ret, Func, Args...>::value;

  // Calculates whether a given function object could throw an exception when called with
  // the sequence of provided arguments, and if the return of said function is implicitly
  // convertible to the given return type, filtering out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Ret, class Func, class... Args>
  struct is_nothrow_invocable_r : detail::is_nothrow_invocable_r_t<Ret, Func, Args...> {
    using canonical_type = std::bool_constant<detail::is_nothrow_invocable_r_t<Ret, Func, Args...>::value>;
  };
  template <class Ret, class Func, class... Args>
  constexpr auto is_nothrow_invocable_r_v = is_nothrow_invocable_r<Ret, Func, Args...>::value;

  namespace detail {
    template <template <class...> class Predicate, class Func, class... Args>
    using is_unwrappable_t = std::disjunction<
      Predicate<Func, Args...>,
      Predicate<Func, filter_none_t<Args...>>
    >;
    template <template <class...> class Predicate, class Ret, class Func, class... Args>
    using is_unwrappable_r_t = std::disjunction<
      Predicate<Ret, Func, Args...>,
      Predicate<Ret, Func, filter_none_t<Args...>>
    >;
  }

  // Calculates whether a given function object is callable with the given argument,
  // or unwrapped sequence of arguments if the argument is a tuple, filtering out any
  // instances of none.
  template <class Func, class Arg>
  struct is_applicable : is_invocable<Func, Arg> {
    using typename is_invocable<Func, Arg>::canonical_type;
  };
  template <class Func, class... Args>
  struct is_applicable<Func, std::tuple<Args...>> :
    detail::is_unwrappable_t<is_invocable, Func, Args...>
  {
    using canonical_type =
      std::bool_constant<detail::is_unwrappable_t<is_invocable, Func, Args...>::value>;
  };
  template <class Func, class Arg>
  constexpr auto is_applicable_v = is_applicable<Func, Arg>::value;

  // Calculates whether a given function could throw an exception when called with the
  // given argument, or unwrapped sequence of arguments if the argument is a tuple,
  // filtering out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Func, class Arg>
  struct is_nothrow_applicable : is_nothrow_invocable<Func, Arg> {
    using typename is_nothrow_invocable<Func, Arg>::canonical_type;
  };
  template <class Func, class... Args>
  struct is_nothrow_applicable<Func, std::tuple<Args...>> :
    detail::is_unwrappable_t<is_nothrow_invocable, Func, Args...>
  {
    using canonical_type =
      std::bool_constant<detail::is_unwrappable_t<is_nothrow_invocable, Func, Args...>::value>;
  };
  template <class Func, class Arg>
  constexpr auto is_nothrow_applicable_v = is_nothrow_applicable<Func, Arg>::value;

  // Calculates whether a given function object returns a type implicitly convertible to
  // the given return type when called with the provided argument, or unwrapped sequence
  // of arguments if the argument is a tuple, filtering out any instances of none.
  // Returns false if the function isn't invocable as specified.
  template <class Ret, class Func, class Arg>
  struct is_applicable_r : is_invocable_r<Ret, Func, Arg> {
    using typename is_invocable_r<Ret, Func, Arg>::canonical_type;
  };
  template <class Ret, class Func, class... Args>
  struct is_applicable_r<Ret, Func, std::tuple<Args...>> :
    detail::is_unwrappable_r_t<is_invocable_r, Ret, Func, Args...>
  {
    using canonical_type =
      std::bool_constant<detail::is_unwrappable_r_t<is_invocable_r, Ret, Func, Args...>::value>;
  };
  template <class Ret, class Func, class Arg>
  constexpr auto is_applicable_r_v = is_applicable_r<Ret, Func, Arg>::value;

  template <class Ret, class Func, class Arg>
  struct is_nothrow_applicable_r : is_nothrow_invocable_r<Ret, Func, Arg> {
    using typename is_nothrow_invocable_r<Ret, Func, Arg>::canonical_type;
  };
  template <class Ret, class Func, class... Args>
  struct is_nothrow_applicable_r<Ret, Func, std::tuple<Args...>> :
    detail::is_unwrappable_r_t<is_nothrow_invocable_r, Ret, Func, Args...>
  {
    using canonical_type =
      std::bool_constant<detail::is_unwrappable_r_t<is_nothrow_invocable_r, Ret, Func, Args...>::value>;
  };
  template <class Ret, class Func, class Arg>
  constexpr auto is_nothrow_applicable_r_v = is_nothrow_applicable_r<Ret, Func, Arg>::value;

  // Invoke implementation.
  namespace detail {
    template <class Functor, class Tuple, size_t... idxs>
    decltype(auto) invoke_impl(Functor&& func, Tuple&& args, std::index_sequence<idxs...>) {
      // A lambda to perform our invocation
      // so we don't have to write this bad boy multiple times.
      auto invocation = [&] () -> decltype(auto) {
        return std::invoke(std::forward<Functor>(func), std::get<idxs>(std::move(args))...);
      };

      // If our invocation will produce some non-void value, return that value.
      // Otherwise rewrite the return into daggr::meta::none.
      if constexpr (!std::is_void_v<decltype(invocation())>) {
        return invocation();
      } else {
        return invocation(), none {};
      }
    }
  }

  // Function takes any kind of callable, and applies the given sequence of arguments,
  // filtering out any instance of none.
  template <class Functor, class... Args, class =
    std::enable_if_t<
      meta::is_invocable_v<Functor, Args...>
    >
  >
  decltype(auto) invoke(Functor&& func, Args&&... the_args) {
    using idxs = filter_none_idxs_t<Args...>;
    return detail::invoke_impl(std::forward<Functor>(func),
        std::forward_as_tuple(std::forward<Args>(the_args)...), idxs {});
  }

  namespace detail {
    template <class Functor, class Tuple, size_t... idxs>
    decltype(auto) apply_impl(Functor&& func, Tuple&& args, std::index_sequence<idxs...>) {
      return meta::invoke(std::forward<Functor>(func), std::get<idxs>(std::forward<Tuple>(args))...);
    }
  }

  template <class Functor, class Arg, class =
    std::enable_if_t<
      meta::is_applicable_v<Functor, Arg>
    >
  >
  decltype(auto) apply(Functor&& func, Arg&& argument) {
    // If our argument is a tuple...
    if constexpr (meta::is_tuple_v<std::decay_t<Arg>>) {
      // We forward and destructure that tuple such that
      // any instances of meta::none will be removed.
      constexpr auto tuple_size = std::tuple_size<std::decay_t<Arg>>::value;
      return detail::apply_impl([&] (auto&&... args) -> decltype(auto) {
        // If our functor can be invoked with our destructured arguments directly...
        if constexpr (meta::is_invocable_v<Functor, decltype(args)...>) {
          // Do so.
          return meta::invoke(std::forward<Functor>(func), std::forward<decltype(args)>(args)...);
        } else {
          // Otherwise, fall back and forward our arguments into an edited tuple.
          return meta::invoke(std::forward<Functor>(func), std::tuple {std::forward<decltype(args)>(args)...});
        }
      }, std::forward<Arg>(argument), std::make_index_sequence<tuple_size> {});;
    } else {
      // Otherwise just invoke our functor with whatever we were given.
      static_assert(meta::is_invocable_v<Functor, Arg>);
      return meta::invoke(std::forward<Functor>(func), std::forward<Arg>(argument));
    }
  }

  // Function calculates the return type of any sequence of arguments applied to any functor.
  // Invocation must be known to be well formed prior to instantiation.
  template <class Functor, class... Args>
  struct invoke_result {
    template <class F, class... As>
    static decltype(meta::invoke(std::declval<Functor>(), std::declval<As>()...)) invocation_calculation(std::true_type);
    template <class, class...>
    static nonesuch invocation_calculation(std::false_type);
    using type = decltype(invocation_calculation<Functor, Args...>(meta::is_invocable<Functor, Args...> {}));
  };
  template <class Functor, class... Args>
  using invoke_result_t = typename invoke_result<Functor, Args...>::type;

  template <class Functor, class Arg>
  struct apply_result {
    template <class F, class A>
    static decltype(meta::apply(std::declval<Functor>(), std::declval<Arg>())) application_calculation(std::true_type);
    template <class, class>
    static nonesuch application_calculation(std::false_type);
    using type = decltype(application_calculation<Functor, Arg>(meta::is_applicable<Functor, Arg> {}));
  };
  template <class Functor, class Arg>
  using apply_result_t = typename apply_result<Functor, Arg>::type;

  namespace detail {

    // Meta-function to calculate whether an operation is defined for all members
    // of some arbitrary sequence.
    template <template <class...> class Validation,
             template <class...> class Calculation, class Success, class Argument, class... Functors>
    struct sequence_is_walkable_impl;

    // Case: The previous operation was ill-formed, sequence is invalid.
    template <template <class...> class Validation,
             template <class...> class Calculation, class Argument, class... Functors>
    struct sequence_is_walkable_impl<Validation, Calculation, std::false_type, Argument, Functors...> : std::false_type {
      using result_type = Argument;
    };

    // Case: Sequence is fully consumed, all operations were well-formed. Sequence is valid.
    template <template <class...> class Validation, template <class...> class Calculation, class Argument>
    struct sequence_is_walkable_impl<Validation, Calculation, std::true_type, Argument> : std::true_type {
      using result_type = Argument;
    };

    // Case: We must attempt to perform the next operation in the sequence, general recursive case.
    template <template <class...> class Validation,
             template <class...> class Calculation, class Argument, class Functor, class... Functors>
    struct sequence_is_walkable_impl<Validation, Calculation, std::true_type, Argument, Functor, Functors...> :
      sequence_is_walkable_impl<
        Validation,
        Calculation,
        typename Validation<Functor, Argument>::canonical_type,
        typename Calculation<Functor, Argument>::type,
        Functors...
      >
    {
      using result_type =
        typename sequence_is_walkable_impl<
          Validation, 
          Calculation,
          typename Validation<Functor, Argument>::canonical_type,
          typename Calculation<Functor, Argument>::type,
          Functors...
        >::result_type;
    };

  }

  // Meta-function calculates whether some abstract operation is valid across
  // a sequence of function types.
  template <template <class...> class Validation,
           template <class...> class Calculation, class Argument, class... Functors>
  struct sequence_is_walkable :
    detail::sequence_is_walkable_impl<
      Validation,
      Calculation,
      std::true_type,
      Argument,
      Functors...
    >
  {
    using result_type =
      typename detail::sequence_is_walkable_impl<
        Validation,
        Calculation,
        std::true_type, 
        Argument,
        Functors...
      >::result_type;
  };
  template <template <class...> class Validation,
           template <class...> class Calculation, class Argument, class... Functors>
  constexpr auto sequence_is_walkable_v =
      sequence_is_walkable<Validation, Calculation, Argument, Functors...>::value;

  template <template <class...> class Validation,
           template <class...> class Calculation, class Argument, class... Functors>
  struct sequence_is_walkable_result {
    using type =
      typename sequence_is_walkable<Validation, Calculation, Argument, Functors...>::result_type;
  };
  template <template <class...> class Validation,
           template <class...> class Calculation, class Argument, class... Functors>
  using sequence_is_walkable_result_t =
      typename sequence_is_walkable_result<Validation, Calculation, Argument, Functors...>::type;

  // Meta-function calculates whether a sequence of function types can be invoked
  // in sequence, passing the result of each to the next, while editing out meta::none
  // and void types when they appear.
  template <class Argument, class... Functors>
  struct sequence_is_invocable :
    sequence_is_walkable<
      meta::is_invocable,
      meta::invoke_result,
      Argument,
      Functors...
    >
  {};
  template <class Argument, class... Functors>
  constexpr auto sequence_is_invocable_v = sequence_is_invocable<Argument, Functors...>::value;

  template <class Argument, class... Functors>
  struct sequence_is_invocable_result {
    using type = typename sequence_is_invocable<Argument, Functors...>::result_type;
  };
  template <class Argument, class... Functors>
  using sequence_is_invocable_result_t = typename sequence_is_invocable_result<Argument, Functors...>::type;

  // Meta-function calculates whether a sequence of function types can be applied
  // in sequence, passing the result of each to the next, while editing out meta::none
  // and void types when they appear, with some custom transformations defined around std::tuple.
  template <class Argument, class... Functors>
  struct sequence_is_applicable :
    sequence_is_walkable<
      meta::is_applicable,
      meta::apply_result,
      Argument,
      Functors...
    >
  {};
  template <class Argument, class... Functors>
  constexpr auto sequence_is_applicable_v = sequence_is_applicable<Argument, Functors...>::value;

  template <class Argument, class... Functors>
  struct sequence_is_applicable_result {
    using type = typename sequence_is_applicable<Argument, Functors...>::result_type;
  };
  template <class Argument, class... Functors>
  using sequence_is_applicable_result_t = typename sequence_is_applicable_result<Argument, Functors...>::type;

  namespace detail {
    template <class T>
    struct type_tag {
      using type = T;
    };

    template <class... Ts, size_t... idxs, class Callback>
    void for_each_t_impl(Callback&& cb, std::index_sequence<idxs...>) {
      (cb(std::integral_constant<size_t, idxs> {}, type_tag<Ts> {}), ...);
    }
  }

  template <class... Ts, class Callback>
  void for_each_t(Callback&& cb) {
    detail::for_each_t_impl<Ts...>(cb, std::index_sequence_for<Ts...> {});
  }

}

#endif
