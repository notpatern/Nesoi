using UnityEngine.InputSystem;
using UnityEngine;
using UnityEngine.UI;

public class BaseMovement : MonoBehaviour , IDamageable
{
    [Header("ref")]
    Rigidbody rb;
    ParryMechanic pm;
    EndState endState;
    
    [SerializeField] private Image dashCoolDownUi;
    [SerializeField] private Image healthBarUi;
    [SerializeField] float health;
    [SerializeField] int damage;
    [SerializeField] Transform cameraHolderTransform;
    [SerializeField] Transform playerModel;
    [SerializeField] Transform orientation;
    [SerializeField] PlayerInputs playerControls;
    [SerializeField] LayerMask isGround;
    [SerializeField] float currentPlayerHeight;
    [SerializeField] Transform Player;
    [SerializeField] Animator animator;
    [SerializeField] AudioSource blade;

    [Header("Values")]
    [SerializeField] float moveSpeed;
    [SerializeField] float jumpStrength;
    [SerializeField] float maxMoveSpeed;
    [SerializeField] float airMultiplierValue;
    [SerializeField] float groundDrag;
    [SerializeField] float airDrag;
    public float dashStrength;
    public float dashDrag;
    public float dashDuration;
    [SerializeField] float stamina;
    float currentStam;
    float currentDashDuration;
    bool dashing;
    bool animDash;
    bool hasDashed;
    bool grounded;
    bool canDamage;
    
    Vector3 moveDirection;
    private float airMultiplier;
    private InputAction move;
    private InputAction vertical;
    private InputAction dash;
    private InputAction parry;

    private void Awake()
    {
        playerControls = new PlayerInputs();
    }

    private void OnEnable()
    {
        move = playerControls.Player.Move;
        move.Enable();

        vertical = playerControls.Player.Vertical;
        vertical.Enable();

        dash = playerControls.Player.Dash;
        dash.Enable();
        dash.performed += Dash;

        parry = playerControls.Player.Parry;
        parry.Enable();
        parry.performed += Parry;
    }
    
    private void OnDisable()
    {
        move.Disable();
        vertical.Disable();
        dash.Disable();
        parry.Disable();
    }

    void Start()
    {
        pm = GetComponent<ParryMechanic>();
        rb = GetComponent<Rigidbody>();
        rb.freezeRotation = true;
        animDash = false;
        rb.drag = groundDrag;
        currentPlayerHeight = 1;
        currentDashDuration = dashDuration;
        grounded = false;
        dashing = false;
        hasDashed = false;
    }

    void Update()
    {
        CheckDeath();
        if (endState != null) endState.CustomUpdate();
        HealthUi();
        AnimatorManager();
        StaminaManager();
        OrientPlayerModelInMoveDirection();
        CheckGrounded();
        StateMachine();
        DashDuration();
    }

    private void FixedUpdate()
    {
        MoveInputs();
    }

    private void DamagingState()
    {
        Vector3 horVel = new Vector3(rb.velocity.x, 0f, rb.velocity.z);
        switch (Mathf.Round(horVel.magnitude))
        {
            case > 20.5f:
                animator.SetBool("dashBool", true);
                canDamage = true;
                break;
            case < 18.5f when grounded:
                animator.SetBool("dashBool", false);
                canDamage = false;
                break;
        }
    }

    private void StaminaManager()
    {
        UiCoolDown();
        IncreaseCurrentStamina();
    }

    private void UiCoolDown()
    {
        dashCoolDownUi.fillAmount = currentStam / stamina;
    }
    
    private void IncreaseCurrentStamina()
    {
        if (currentStam <= stamina) currentStam += Time.deltaTime;
        else if (currentStam > stamina) currentStam = stamina;
    }

    private void UseCurrentStamina()
    {
        currentStam = 0;
    }

    public int Damage()
    {
        return damage;
    }
    
    public bool CanDamage()
    {
        return canDamage;
    }

    private void AnimatorManager()
    {
        float x = move.ReadValue<Vector2>().x;
        float y = move.ReadValue<Vector2>().y;
        
        DamagingState();
        if ((x != 0 || y != 0) && !animDash && grounded) animator.SetBool("walkBool", true);
        else if ((x == 0 && y == 0) || animDash || !grounded) animator.SetBool("walkBool", false);
    }

    private void StateMachine()
    {
        if (pm.isParrying) pm.ParryBehavior();
        if (pm.parrySuccessful) currentStam = stamina;
        if (grounded && !dashing)
        {
            rb.drag = groundDrag;
            airMultiplier = 1f;
            rb.useGravity = true;
        }
        if ((dashing) || (grounded && dashing))
        {
            rb.drag = dashDrag;
            rb.useGravity = false;
        }
        if (!dashing && !hasDashed) animDash = false;
        if (dashing || grounded) return;
        rb.drag = airDrag;
        airMultiplier = airMultiplierValue;
        rb.useGravity = true;
    }

    private void CheckGrounded()
    {
        grounded = Physics.Raycast(transform.position, Vector3.down, currentPlayerHeight + 0.2f, isGround);
        if (!grounded) return;
        if (hasDashed)
        {
            hasDashed = false;
        }
    }

    private void Jump()
    {
        rb.velocity = new Vector3(rb.velocity.x, 0f, rb.velocity.z);
        rb.AddForce(transform.up * jumpStrength, ForceMode.Impulse);
    }

    private void MoveInputs()
    {
        float x = move.ReadValue<Vector2>().x;
        float y = move.ReadValue<Vector2>().y;

        Vector2 mag = FindVelRelativeToLook();
        float xMag = mag.x, yMag = mag.y;

        if (x > 0 && xMag > maxMoveSpeed) x = 0;
        if (x < 0 && xMag < -maxMoveSpeed) x = 0;
        if (y > 0 && yMag > maxMoveSpeed) y = 0;
        if (y < 0 && yMag < -maxMoveSpeed) y = 0;

        moveDirection = orientation.forward * y + orientation.right * x;
        rb.AddForce(moveDirection.normalized * (10f * moveSpeed * airMultiplier), ForceMode.Acceleration);

        if (vertical.ReadValue<float>() > 0 & grounded)
        {
            Jump();
        }
    }

    private Vector2 FindVelRelativeToLook()
    {
        float lookAngle = orientation.transform.eulerAngles.y;
        float moveAngle = Mathf.Atan2(rb.velocity.x, rb.velocity.z) * Mathf.Rad2Deg;

        float u = Mathf.DeltaAngle(lookAngle, moveAngle);
        float v = 90 - u;

        float magnitue = rb.velocity.magnitude;
        float yMag = magnitue * Mathf.Cos(u * Mathf.Deg2Rad);
        float xMag = magnitue * Mathf.Cos(v * Mathf.Deg2Rad);

        return new Vector2(xMag, yMag);
    }

    private void Dash(InputAction.CallbackContext context)
    {
        if (hasDashed) return;
        if (currentStam < stamina) return;
        
        hasDashed = true;
        dashing = true;
        if (dashing && !animDash)
        {
            animDash = true;
        }
        float x = move.ReadValue<Vector2>().x;
        blade.Play();
        UseCurrentStamina();
        var dashDirection = cameraHolderTransform.forward;
        rb.velocity = new Vector3(0f, 0f, 0f);
        rb.AddForce(dashDirection * dashStrength, ForceMode.Impulse);
    }
    
    private void DashDuration()
    {
        if (dashing && currentDashDuration > 0)
        {
            currentDashDuration  -= Time.deltaTime;
        }
        if (vertical.ReadValue<float>() > 0 && grounded)
        {
            dashing = false;
            currentDashDuration  = dashDuration;
        }

        if (!dashing || !(currentDashDuration <= 0)) return;
        dashing = false;
        currentDashDuration = dashDuration;
    }

    private void Parry(InputAction.CallbackContext context)
    {
        if (!pm.canParry) return;
        if (dashing) return;
        blade.Play();
        pm.StartParry(animator);
    }

    private void OrientPlayerModelInMoveDirection()
    {
        var currentHorVel = new Vector3(rb.velocity.x, 0f, rb.velocity.z);
        if (currentHorVel.magnitude <= 0.3f)
            return;
        playerModel.forward = -currentHorVel;
    }

    public void TakeDamage(int damage)
    {
        health -= damage;
        if (health > 0) return;
    }

    private void HealthUi()
    {
        healthBarUi.fillAmount = health / 6;
    }

    private void CheckDeath()
    {
        if (endState != null) return;
        if (health <= 0) endState = new EndState(0.5f);
    }
}
