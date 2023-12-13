using UnityEngine;
public class ParryMechanic : MonoBehaviour
{
    [SerializeField] private Collider parryArea;
    [SerializeField] private Transform cameraHolder;
    [SerializeField] private float parryTime;
    [SerializeField] private float parryCoolDown;
    [SerializeField] private float parryForce;
    private float currentParryTime;
    public bool canParry;
    public bool isParrying;
    public bool parrySuccessful;
    private Animator animator;

    private void Start()
    {
        canParry = true;
        isParrying = false;
        parryArea.gameObject.SetActive(false);
    }

    public void StartParry(Animator animator)
    {
        this.animator = animator;
        this.animator.SetBool("parryBool", true);
        canParry = false;
        isParrying = true;
        parryArea.gameObject.SetActive(true);
        currentParryTime = parryTime;
        ParryBehavior();
    }

    public void ParryBehavior()
    {
        currentParryTime -= Time.deltaTime;
        if (!(currentParryTime <= 0)) return;
        isParrying = false;
        EndParry();
    }

    private void EndParry()
    {
        animator.SetBool("parryBool", false);
        parrySuccessful = false;
        parryArea.gameObject.SetActive(false);
        Invoke(nameof(ResetParry), parryCoolDown);
    }

    private void ResetParry()
    {
        canParry = true;
    }

    private void OnTriggerEnter(Collider other)
    {
        if (!isParrying) return;
        if (other.CompareTag("bullet")) ParryProjectile(other);
        if (other.CompareTag("melee")) ParryMelee(other);
    }

    private void ParryProjectile(Collider other)
    {
        parrySuccessful = true;
        var target = other.GetComponent<DealDamage>();
        if (target == null) return;
        target.canDamage = true;
        target.parent = cameraHolder;
        target.rb.velocity = Vector3.zero;
        target.rb.AddForce(cameraHolder.forward * parryForce, ForceMode.Impulse);
    }

    private void ParryMelee(Collider other)
    {
        var target = other.GetComponent<IDamageable>();
        if (target == null) return;
        target.TakeDamage(target.Damage());
        parrySuccessful = true;
    }
}
